package skald 

import java.io.File
import java.nio.file.{Path => JPath, Paths, Files => JFiles}
import scala.sys.process.*
import skald.JobManager.BackgroundJob
import skald.Files.readFromFile

case class CommandResult(stdout: String, stderr: String = "", exitCode: Int = 0)

object Executor {

  def run(cmd: Command, env: ShellEnv): Unit = 
    evaluate(cmd, env, None, false)

  def evaluate(
    cmd: Command, 
    env: ShellEnv = ShellEnv(),
    stdin: Option[String] = None, 
    isSilent: Boolean = false
  ): (CommandResult, ShellEnv) = cmd match {


    case Exit => 
      System.exit(0)
      (CommandResult(""), env)

    case Echo(args) => 
      val out = args.mkString(" ") + "\n"
      shouldPrint(out, isSilent)
      (CommandResult(out), env)

    case Pwd() => 
      val out = env.cwd.toString() + "\n"
      shouldPrint(out, isSilent) 
      (CommandResult(out), env)

    case Cd(args) => 
      handleCd(args.headOption.getOrElse("~"), env.cwd) match {
        case Right(newPath) => (CommandResult(""), env.withCwd(newPath))
        case Left(err)      => 
          val out = err + "\n"
          shouldPrint(out, isSilent)
          (CommandResult("", out, 1), env)
      }

    case Type(args) => 
      val out = handleType(args.headOption.getOrElse("")) + "\n"
      shouldPrint(out, isSilent)
      (CommandResult(out), env)

    case Complete(args) =>
      args match {
        case PrintSpec(cmd) => 
          CompletionRegistry.get(cmd) match {
            case Some(path) => 
              val out = s"complete -C '$path' $cmd\n"
              shouldPrint(out, isSilent)
              (CommandResult(out), env)
            case None =>
              val out = s"complete: $cmd: no completion specification\n"
              shouldPrint(out, isSilent)
              (CommandResult(out), env)
          }
        case RegisterSpec(path, cmd) => 
          CompletionRegistry.register(path, cmd)
          (CommandResult(""), env)

        case UnregisterSpec(cmd) =>
          CompletionRegistry.unregister(cmd)
          (CommandResult(""), env)
         
      }

    case Jobs() =>
      JobManager.jobTable.values.toList.sortBy(_.id).foreach { job =>
        JobManager.printJob(job)
        if (!job.process.isAlive) JobManager.removeJob(job.id)
      }
      (CommandResult(""), env)

    case History(arg) =>
        arg match {
          case nHistory(n) => 
            val out = HistoryManager.showHistory(n)
            shouldPrint(out, isSilent)
            (CommandResult(out), env)
          case ReadFromFile(file) => 
            HistoryManager.readFromFile(file)
            (CommandResult(""), env)
          case WriteToFile(file) =>
            HistoryManager.writeToFile(file)
            (CommandResult(""), env)
          case AppendToFile(file) =>
            HistoryManager.appendToFile(file)
            (CommandResult(""), env)

          case ShowAll => 
            val out = HistoryManager.showHistory()
            shouldPrint(out, isSilent)
            (CommandResult(out), env)
        }

    case Declare(arg) =>
      arg match {
        case PrintVariable(variable) => 
          val res = env.format(variable).fold(
            err => {
              val outerr = err + "\n"
              shouldPrint(outerr, isSilent)
              CommandResult(stdout = "", stderr = outerr, exitCode = 1)
            },
            out => {
              val outstr = out + "\n"
              shouldPrint(outstr, isSilent)
              CommandResult(out)
            }
          )
          (res, env)
        
        case AssignVariable(variable, value) =>
          val nextEnv = env.setVariable(variable, value)
          (CommandResult(""), nextEnv)
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

          val newJob = BackgroundJob(jobId, pid, s"$name ${args.mkString(" ")}", process)
          JobManager.addJob(newJob)

          System.out.println(s"[$jobId] $pid")
          (CommandResult(""), env)
        case _ =>
          System.out.println(s"[$jobId] (builtin)")
          new Thread(() => run(cmd, env)).start()
          (CommandResult(""), env)
      }

    case External(name, args) => 
      val res = runExternal(name, args, env, stdin)
      if (!isSilent) {
        if (res.stdout.nonEmpty) {
          System.out.print(res.stdout)
          System.out.flush()
        }
        if (res.stderr.nonEmpty) {
          System.err.print(res.stderr)
          System.err.flush()
        }
      }
      (res, env)

    case Pipeline(commands) => 
      val allExternals = commands.forall(_.isInstanceOf[External])
      if (allExternals && commands.nonEmpty) {
        val externals = commands.collect { case e: External => e }
        runPipeline(externals, env)
      } else {
        executeChain(commands, stdin, env)
      }
      (CommandResult(""), env)

      
    case RedirectStdout(cmd, targetFile) => 
      val (res, nextEnv) = evaluate(cmd, env, stdin, isSilent = true)
      Files.writeToFile(targetFile, res.stdout)
      if (res.stderr.nonEmpty) {
        System.out.print(res.stderr)
        System.out.flush()
      }
      (res.copy(stdout = ""), nextEnv) 

    case RedirectStderr(cmd, targetFile) =>
      val (res, nextEnv) = evaluate(cmd, env, stdin, isSilent = true)
      Files.writeToFile(targetFile, res.stderr)
      if (res.stdout.nonEmpty) {
        System.out.print(res.stdout)
        System.out.flush()
      }
      (res.copy(stderr = ""), nextEnv)

    case AppendStdout(cmd, targetFile) =>
      val (res, nextEnv) = evaluate(cmd, env, stdin, isSilent = true)
      Files.appendToFile(targetFile, res.stdout)
      if (res.stderr.nonEmpty) {
        System.out.print(res.stderr)
        System.out.flush()
      }
      (res.copy(stdout = ""), nextEnv)

    case AppendStderr(cmd, targetFile) =>
      val (res, nextEnv) = evaluate(cmd, env, stdin, isSilent = true)
      Files.appendToFile(targetFile, res.stderr)
      if (res.stdout.nonEmpty) {
        System.out.print(res.stdout)
        System.out.flush()
      }
      (res.copy(stderr = ""), nextEnv)
  }

  private def executeChain(commands: List[Command], stdin: Option[String], initialEnv: ShellEnv): (CommandResult, ShellEnv) = {
    val startValue = (CommandResult("", "", 0), initialEnv)

    commands.foldLeft(startValue) { case ((acc, env), cmd) =>
      val currentStdin = if (acc.stdout.isEmpty) stdin else Some(acc.stdout)
      val (nextResult, nextEnv) = evaluate(cmd, env, currentStdin, isSilent = (cmd != commands.last))
      (nextResult, nextEnv)
    }
  }



  /* stdin tilføjet i tilfælde af at external er højre led i en pipeline, i så fald vil stdin beskrive
   * output fra venstre led
  */
  private def runExternal(name: String, args: List[String], env: ShellEnv, stdin: Option[String] = None): CommandResult = {
    Path.findInPath(name) match {
      case Some(fullPath) =>

        val pb = new java.lang.ProcessBuilder((name :: args)*)
        pb.directory(env.cwd.toFile)
        val process = pb.start()

        stdin.foreach { input =>
          val writer = new java.io.BufferedWriter(new java.io.OutputStreamWriter(process.getOutputStream))
          writer.write(input)
          writer.flush()
          writer.close()
        }

        val out = new StringBuilder()
        val outReader = new java.io.BufferedReader(new java.io.InputStreamReader(process.getInputStream))
        var line: String = null
        while ({ line = outReader.readLine(); line != null }) {
          out.append(line).append("\n")
        }

        val err = new StringBuilder()
        val errReader = new java.io.BufferedReader(new java.io.InputStreamReader(process.getErrorStream))
        while ({ line = errReader.readLine(); line != null }) {
          err.append(line).append("\n")
        }

        val exitCode = process.waitFor()
        CommandResult(out.toString(), err.toString(), exitCode)
        
      case None => CommandResult("", s"$name: not found\n", 127)
    }
  }

  private def runPipeline(commands: List[External], env: ShellEnv): CommandResult = {
    import scala.jdk.CollectionConverters._

    val builders = commands.map { cmd =>
      val pb = new java.lang.ProcessBuilder((cmd.name :: cmd.args)*)
      pb.directory(env.cwd.toFile())
      pb
    }

    val processes = java.lang.ProcessBuilder.startPipeline(builders.asJava).asScala.toList
    val lastProcess = processes.last

    val output = readAndStream(lastProcess)
    processes.foreach(_.waitFor())

    CommandResult(output)
  }



  private def readAndStream(process: java.lang.Process): String = {
    val reader = new java.io.BufferedReader(new java.io.InputStreamReader(process.getInputStream))
    val sb = new StringBuilder()
    var line: String = null
    
    while ({ line = reader.readLine(); line != null }) {
      System.out.println(line) 
      System.out.flush()
      sb.append(line).append("\n")
    }
    sb.toString()
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


  private def shouldPrint(str: String, check: Boolean) =
    if (!check) {
      System.out.print(str)
      System.out.flush()
    }
}
