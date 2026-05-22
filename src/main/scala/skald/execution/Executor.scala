package skald 

import java.io.File
import java.nio.file.{Path => JPath, Paths, Files => JFiles}
import scala.sys.process.*
import skald.JobManager.BackgroundJob
import skald.Files.readFromFile

case class CommandResult(stdout: String, stderr: String = "", exitCode: Int = 0)

object Executor {

  def run(cmd: Command, env: ShellEnv): Unit = {
    evaluate(cmd, env, Iterator.empty, false)
  }

  def evaluate(
    cmd: Command, 
    env: ShellEnv = ShellEnv(),
    stdin: Iterator[ShellData] = Iterator.empty, 
    isSilent: Boolean = false
  ): (ExecutionResult, ShellEnv) = cmd match {


    case Exit => 
      System.exit(0)
      (ExecutionResult(Iterator.empty), env)

    case Echo(args) => 
      val out = args.mkString(" ")
      val data = Iterator(ShellData.Text(out))
      (ExecutionResult(data), env)

    case Pwd() => 
      val data = Iterator(ShellData.Text(env.cwd.toString()))
      (ExecutionResult(data), env)

    case Cd(args) => 
      handleCd(args.headOption.getOrElse("~"), env.cwd) match {
        case Right(newPath) => (ExecutionResult(Iterator.empty), env.withCwd(newPath))
        case Left(err)      => 
          val errStr = err + "\n"
          printError(errStr, isSilent)
          (ExecutionResult(Iterator.empty, stderr = errStr, 1), env)
      }

    case Type(args) => 
      val out = handleType(args.headOption.getOrElse(""))
      val data = Iterator(ShellData.Text(out))
      (ExecutionResult(Iterator.empty), env)

    case Complete(args) =>
      args match {
        case PrintSpec(cmd) => 
          CompletionRegistry.get(cmd) match {
            case Some(path) => 
              val out = s"complete -C '$path' $cmd\n"
              val data = Iterator(ShellData.Text(out))
              (ExecutionResult(data), env)
            case None =>
              val out = s"complete: $cmd: no completion specification\n"
              printError(out, isSilent)
              (ExecutionResult(Iterator.empty, stderr = out), env)
          }
        case RegisterSpec(path, cmd) => 
          CompletionRegistry.register(path, cmd)
          (ExecutionResult(Iterator.empty), env)

        case UnregisterSpec(cmd) =>
          CompletionRegistry.unregister(cmd)
          (ExecutionResult(Iterator.empty), env)
         
      }

    case Jobs() =>
      JobManager.jobTable.values.toList.sortBy(_.id).foreach { job =>
        JobManager.printJob(job)
        if (!job.process.isAlive) JobManager.removeJob(job.id)
      }
      (ExecutionResult(Iterator.empty), env)

    case History(arg) =>
        arg match {
          case nHistory(n) => 
            val out = HistoryManager.showHistory(n)
            val data = Iterator(ShellData.Text(out))
            (ExecutionResult(data), env)
          case ReadFromFile(file) => 
            HistoryManager.readFromFile(file)
            (ExecutionResult(Iterator.empty), env)
          case WriteToFile(file) =>
            HistoryManager.writeToFile(file)
            (ExecutionResult(Iterator.empty), env)
          case AppendToFile(file) =>
            HistoryManager.appendToFile(file)
            (ExecutionResult(Iterator.empty), env)

          case ShowAll => 
            val out = HistoryManager.showHistory()
            val data = Iterator(ShellData.Text(out))
            (ExecutionResult(data), env)
        }

    case Declare(arg) =>
      arg match {
        case PrintVariable(variable) => 
          val res = env.format(variable).fold(
            err => {
              val outerr = err + "\n"
              printError(outerr, isSilent)
              ExecutionResult(Iterator.empty, stderr = outerr, exitCode = 1)
            },
            out => {
              val outstr = out + "\n"
              val data = Iterator(ShellData.Text(outstr))
              ExecutionResult(data)
            }
          )
          (res, env)
        
        case AssignVariable(variable, value) =>
          val nextEnv = env.setVariable(variable, value)
          (ExecutionResult(Iterator.empty), nextEnv)
      }

    case Subprocess(cmd) =>
      val jobId = JobManager.nextJobId

      cmd match {
        case External(name, args) =>
          val pb = new java.lang.ProcessBuilder((name :: args)*)
          pb.directory(env.cwd.toFile())
          pb.inheritIO()
          val process = pb.start()
          val pid = process.pid()

          val cmdLine = s"$name ${args.mkString(" ")}"
          val newJob = BackgroundJob(jobId, pid, cmdLine, process)
          JobManager.addJob(newJob)

          System.out.println(s"[$jobId] $pid")
          val data = Iterator(ShellData.ProcessInfo(pid.toInt, cmdLine))
          (ExecutionResult(data), env)
        case _ =>
          System.out.println(s"[$jobId] (builtin)")
          new Thread(() => run(cmd, env)).start()
          val data = Iterator(ShellData.ProcessInfo(0, "(builtin)"))
          (ExecutionResult(data), env)
      }

    case External(name, args) => 
      val res = runExternal(name, args, env, stdin)
      printError(res.stderr, isSilent)
      (res, env)

    case Pipeline(commands) => executeChain(commands, stdin, env)

    case Redirect(cmd, target, mode, targetFile) =>
      val (res, nextEnv) = evaluate(cmd, env, stdin, isSilent = true)

      println(s"DEBUG: stdout length = ${res.output.size}")
      println(s"DEBUG: stderr content = '${res.stderr}'")

      val dataToFile = target match {
        case Stdout => res.output.map(_.asString).mkString("\n") + "\n"
        case Stderr => res.stderr
      }

      mode match {
        case Overwrite => Files.writeToFile(targetFile, dataToFile)
        case Append => Files.appendNewlineToFile(targetFile, dataToFile)
      }

      target match {
        case Stdout => (res.copy(output = Iterator.empty), nextEnv)
        case Stderr => (res.copy(stderr = ""), nextEnv)
      }
      
    // case RedirectStdout(cmd, targetFile) => 
    //   val (res, nextEnv) = evaluate(cmd, env, stdin, isSilent = true)
    //
    //   val writer = new java.io.BufferedWriter(new java.io.FileWriter(targetFile))
    //   res.output.foreach { item =>
    //     writer.write(item.asString + "\n")
    //   }
    //   writer.close()
    //
    //   if (res.stderr.nonEmpty) {
    //       System.out.print(res.stderr)
    //       System.out.flush()
    //     }
    //
    //   (res.copy(output = Iterator.empty), nextEnv) 
    //
    // case RedirectStderr(cmd, targetFile) =>
    //   val (res, nextEnv) = evaluate(cmd, env, stdin, isSilent = true)
    //   Files.writeToFile(targetFile, res.stderr)
    //   if (res.stdout.nonEmpty) {
    //     System.out.print(res.stdout)
    //     System.out.flush()
    //   }
    //   (res.copy(stderr = ""), nextEnv)
    //
    // case AppendStdout(cmd, targetFile) =>
    //   val (res, nextEnv) = evaluate(cmd, env, stdin, isSilent = true)
    //  
    //   val writer = new java.io.BufferedWriter(new java.io.FileWriter(targetFile, true))
    //   res.output.foreach { item => 
    //     writer.write(item.asString + "\n")
    //   }
    //   writer.close()
    //  
    //   if (res.stderr.nonEmpty) {
    //     System.out.print(res.stderr)
    //     System.out.flush()
    //   }
    //   (res.copy(output = Iterator.empty), nextEnv)
    //
    // case AppendStderr(cmd, targetFile) =>
    //   val (res, nextEnv) = evaluate(cmd, env, stdin, isSilent = true)
    //   Files.appendToFile(targetFile, res.stderr)
    //   if (res.stdout.nonEmpty) {
    //     System.out.print(res.stdout)
    //     System.out.flush()
    //   }
    //   (res.copy(stderr = ""), nextEnv)
  }

  private def executeChain(
    commands: List[Command], 
    stdin: Iterator[ShellData], 
    initialEnv: ShellEnv
  ): (ExecutionResult, ShellEnv) = {
    
    commands.foldLeft((ExecutionResult(stdin), initialEnv)) {
      case ((accRes, currentEnv), cmd) =>
        evaluate(cmd, currentEnv, accRes.output, isSilent = (cmd != commands.last))
    }
  }



  /* stdin tilføjet i tilfælde af at external er højre led i en pipeline, i så fald vil stdin beskrive
   * output fra venstre led
  */
  private def runExternal(name: String, args: List[String], env: ShellEnv, stdin: Iterator[ShellData]): ExecutionResult = {
    Path.findInPath(name) match {
      case Some(fullPath) =>

        val pb = new java.lang.ProcessBuilder((name :: args)*)
        pb.directory(env.cwd.toFile)
        val process = pb.start()

        if (stdin.nonEmpty) {
          new Thread(() => {
            val writer = new java.io.BufferedWriter(new java.io.OutputStreamWriter(process.getOutputStream))
            stdin.foreach { data =>
              writer.write(data.asString + "\n")
            }
            writer.flush()
            writer.close()
          }).start()
        }

        val reader = new java.io.BufferedReader(new java.io.InputStreamReader(process.getInputStream))
        val outputIterator = new Iterator[ShellData] {
          private var nextLine: String = reader.readLine()
          
          override def hasNext: Boolean = nextLine != null
          
          override def next(): ShellData = {
            val current = nextLine
            nextLine = reader.readLine() // Forbered næste kald
            
            // Hvis vi er færdige, vent på at processen dør pænt
            if (nextLine == null) process.waitFor() 
            
            ShellData.Text(current)
          }
        }

        val errorStream = process.getErrorStream()
        val errout = scala.io.Source.fromInputStream(errorStream).mkString

        ExecutionResult(outputIterator, stderr = errout)

      case None => 
        ExecutionResult(Iterator(ShellData.Text(s"$name: not found")), exitCode = 127)
    }
  }

  private def handleType(cmd: String): String = {
    if (Builtin.isBuiltin(cmd)) s"$cmd is a shell builtin"
    else Path.findInPath(cmd).map(p => s"$cmd is $p").getOrElse(s"$cmd: not found")
  }

  private def handleCd(pathStr: String, currentCwd: JPath): Either[String, JPath] = {
    val target = if (pathStr == "~") sys.env.getOrElse("HOME", System.getProperty("user.home")) else pathStr
    val p = Paths.get(target)

    val absP = if (p.isAbsolute) p else currentCwd.resolve(target)
    val normP = absP.normalize()

    if (JFiles.isDirectory(normP)) Right(normP)
    else Left(s"cd: $pathStr: No such file or directory")
  }

  private def printError(err: String, isSilent: Boolean): Unit = {
    if (!isSilent && err.nonEmpty) {
      System.out.print(err)
      System.out.flush()
    }
  }
}
