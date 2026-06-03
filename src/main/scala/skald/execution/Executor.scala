package skald 

import Command._
import RedirectionType._
import RedirectionMode._
import CompleteFlag._
import DeclareFlag._
import HistoryFlag._
import AliasFlag._
import Result._

import java.nio.file.{Path => JPath, Paths, Files => JFiles}
import scala.sys.process.*
import skald.JobManager.BackgroundJob
import skald.Files.readFromFile
import skald.ShellData.GrepMatch
import scala.util.Using

object Executor {

  def run(cmd: Command, env: ShellEnv): Unit = {
    evaluate(cmd, env, Iterator.empty)
  }

  def evaluate(
    cmd: Command, 
    env: ShellEnv = ShellEnv(),
    stdin: Iterator[ShellData] = Iterator.empty, 
    captureOutput: Boolean = false
  ): (ExecutionResult, ShellEnv) = cmd match {


    case Exit => 
      System.exit(0)
      (ExecutionResult(Iterator.empty), env)

    case Echo(args) => 
      val out = args.mkString(" ")
      val data = Iterator(ShellData.Text(out))
      (ExecutionResult(data), env)

    case Pwd => 
      val data = Iterator(ShellData.Text(env.cwd.toString()))
      (ExecutionResult(data), env)

    case Cd(args) => 
      handleCd(args.headOption.getOrElse("~"), env.cwd) match {
        case Right(newPath) => (ExecutionResult(Iterator.empty), env.withCwd(newPath))
        case Left(err)      => (ExecutionResult(Iterator.empty, stderr = err + "\n", 1), env)
      }

    case Ls =>
      val fileNodes = Files.listDirectory(env.cwd).map(ShellData.FileNode(_))
      (ExecutionResult(fileNodes), env)

    // TODO Håndter errors bedre i tilfælde af redirections
    case Cat(files) =>
      if (files.nonEmpty)  { 
        val catIterator = files.iterator.flatMap { file =>
          Files.readFromFile(file) match {
            case Success(lineIte) => lineIte.map(ShellData.FileLine(file, _))
            case Fail(err) => 
              System.err.println(err.printError)
              Iterator.empty
          }
        }
        (ExecutionResult(catIterator), env)
      } else {
        val catIterator = stdin.flatMap { 
          case ShellData.FileNode(path) => 
            Files.readFromFile(path.toString) match {
              case Success(lineIte) => lineIte.map(ShellData.FileLine(path.toString, _))
              case Fail(err) => 
                System.err.println(err.printError)
                Iterator.empty
            }
          case ShellData.Text(str) => 
            Iterator(ShellData.Text(str))

          case other => 
            System.err.println(s"cat: unexpected datatype in pipeline: $other")
            Iterator.empty
        }
        (ExecutionResult(catIterator), env)
      }

    case Grep(word, files) =>
      if (files.isEmpty) {
        val filteredOutput = stdin.zipWithIndex.collect {
          case (ShellData.FileLine(filename, content), idx) if content.contains(word) =>
            ShellData.GrepMatch(Some(filename), idx + 1, content, word)
            
          case (ShellData.Text(content), idx) if content.contains(word) =>
            ShellData.GrepMatch(None, idx + 1, content, word)
        }
        (ExecutionResult(filteredOutput), env)

      } else {
        val (iterators, errors) = files.foldLeft((List.empty[Iterator[ShellData]], List.empty[String])) {
          case ((iters, errs), fileName) =>
            Files.readFromFile(fileName) match {
              case Success(lines) =>
                val showFileName = if (files.size > 1) Some(fileName) else None
                
                val matchedLines = lines.zipWithIndex.collect {
                  case (line, idx) if line.contains(word) =>
                    ShellData.GrepMatch(showFileName, idx + 1, line, word)
                }
                (matchedLines :: iters, errs)
                
              case Fail(err) => 
                (iters, err.printError :: errs)
            }
        }
        val combinedOutput = iterators.reverse.reduceOption(_ ++ _).getOrElse(Iterator.empty)
        val stderrOutput = errors.reverse.mkString("\n")

        (ExecutionResult(output = combinedOutput, stderr = stderrOutput), env)
      }

    case Type(args) => 
      val out = handleType(args.headOption.getOrElse(""))
      val data = Iterator(ShellData.Text(out))
      (ExecutionResult(data), env)

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
              (ExecutionResult(Iterator.empty, stderr = out, exitCode = 1), env)
          }
        case RegisterSpec(path, cmd) => 
          CompletionRegistry.register(path, cmd)
          (ExecutionResult(Iterator.empty), env)

        case UnregisterSpec(cmd) =>
          CompletionRegistry.unregister(cmd)
          (ExecutionResult(Iterator.empty), env)
         
      }

    case Jobs =>
      JobManager.jobTable.values.toList.sortBy(_.id).foreach { job =>
        JobManager.printJob(job)
        if (!job.process.isAlive) JobManager.removeJob(job.id)
      }
      (ExecutionResult(Iterator.empty), env)

    case History(arg) =>
        arg match {
          case NHistory(n) => 
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
          val res = env.formatVariable(variable).fold(
            err => {
              val outerr = err + "\n"
              ExecutionResult(Iterator.empty, stderr = outerr, exitCode = 1)
            },
            out => {
              val outstr = out + "\n" //Nødvendigt? tror ik TODO
              val data = Iterator(ShellData.Text(outstr))
              ExecutionResult(data)
            }
          )
          (res, env)
        
        case AssignVariable(variable, value) =>
          val nextEnv = env.setVariable(variable, value, VarScope.Local)
          (ExecutionResult(Iterator.empty), nextEnv)
      }

    case Export(name, value) =>
      val nextEnv = env.setVariable(name, value, VarScope.Global)
      (ExecutionResult(Iterator.empty), nextEnv)

    case Subprocess(cmd) =>
      val jobId = JobManager.nextJobId

      cmd match {
        case External(name, args) =>
          createProcessBuilder(name, args, env) match {
            case Some(pb) =>
              pb.inheritIO()
              val process = pb.start()
              val pid = process.pid()

              val cmdLine = s"$name ${args.mkString(" ")}"
              val newJob = BackgroundJob(jobId, pid, cmdLine, process)
              JobManager.addJob(newJob)

              System.out.println(s"[$jobId] $pid")
              val data = Iterator(ShellData.ProcessInfo(pid.toInt, cmdLine))
              (ExecutionResult(data), env)
            case None =>
              System.err.println(s"$name: not found")
              (ExecutionResult(Iterator.empty, exitCode = 127), env)
          }
        case _ =>
          System.out.println(s"[$jobId] (builtin)")
          new Thread(() => run(cmd, env)).start()
          val data = Iterator(ShellData.ProcessInfo(0, "(builtin)"))
          (ExecutionResult(data), env)
          
      }

    case External(name, args) => 
      val res = runExternal(name, args, env, stdin, captureOutput)
      (res, env)

    case Pipeline(commands) => executeChain(commands, stdin, env, captureOutput)

    case Redirect(cmd, target, mode, targetFile) =>
      val (res, nextEnv) = evaluate(cmd, env, stdin, true)

      val dataToFile = target match {
        case Stdout => res.output.map(_.asString)
        case Stderr => 
          if (res.stderr.nonEmpty) Iterator(res.stderr)
          else Iterator.empty
      }

      mode match {
        case Overwrite => Files.writeToFile(targetFile, dataToFile)
        case Append => Files.appendToFile(targetFile, dataToFile)
      }

      val finalRes = target match {
        case Stdout => res.copy(output = Iterator.empty)
        case Stderr => res.copy(stderr = "")
      }
      (finalRes, nextEnv)

    case Filter(expr) => 
      val filteredStream = stdin.filter { item =>
        evalExpr(expr, item) match {
          case ShellValue.VBool(true) => true
          case _                      => false
        }
      }
      (ExecutionResult(filteredStream), env)

    case Map(expr) => 
      val mappedStream = stdin.flatMap { item =>
        val evaluatedValue = evalExpr(expr, item)
        valueToData(evaluatedValue)
      }
      (ExecutionResult(mappedStream), env)

    case Sort(expr, descending) =>
      val sortedStream = stdin.toList.sortWith { (left, right) =>
        val leftVal = evalExpr(expr, left)
        val rightVal = evalExpr(expr, right)
        
        compareValues(leftVal, rightVal) match {
          case Some(cmp) => cmp < 0
          case None => false
        }
      }.iterator
      (ExecutionResult(sortedStream), env)

    case Alias(arg) =>
      arg match {
        case AssignAlias(name, value) =>
          val nextEnv = env.setAlias(name, value)
          (ExecutionResult(Iterator.empty), nextEnv)
        case PrintAll => 
          val aliasList = env.aliases.map { case (name, value) =>
            ShellData.Text(s"$name='$value'")
          }.iterator

          (ExecutionResult(aliasList), env)
      }
      
    case Unalias(name) => 
      env.removeAlias(name) match {
        case None => (ExecutionResult(stderr = s"no alias with name $name"), env)
        case Some(nextEnv) => (ExecutionResult(Iterator.empty), nextEnv)
      }

    case Source(file) =>
      val expandedPath = Files.expandPath(file)
      val targetPath = Paths.get(expandedPath)

      val nextEnv = ConfigLoader.loadFromFile(targetPath, env)

      (ExecutionResult(Iterator.empty), nextEnv)
  }

  private def valueToData(v: ShellValue): Option[ShellData] = v match {
    case ShellValue.VLong(l)    => Some(ShellData.Text(l.toString))
    case ShellValue.VString(s)  => Some(ShellData.Text(s))
    case ShellValue.VBool(b)    => Some(ShellData.Text(b.toString))
    case ShellValue.VDate(d)    => Some(ShellData.Text(d.toString))
    case ShellValue.VTime(t)    => Some(ShellData.Text(t.toString))
    case ShellValue.VNone       => None 
  }

  private def executeChain(
    commands: List[Command], 
    stdin: Iterator[ShellData], 
    initialEnv: ShellEnv,
    captureOutput: Boolean
  ): (ExecutionResult, ShellEnv) = {
    
    commands.zipWithIndex.foldLeft((ExecutionResult(stdin), initialEnv)) {
      case ((accRes, currentEnv), (cmd, idx)) =>
        val isLastCommand = idx == commands.size - 1
        val shouldCapture = if (isLastCommand) captureOutput else true

        evaluate(cmd, currentEnv, accRes.output, shouldCapture)
    }
  }


  /* stdin tilføjet i tilfælde af at external er højre led i en pipeline, i så fald vil stdin beskrive
   * output fra venstre led
  */
  private def runExternal(name: String, args: List[String], env: ShellEnv, stdin: Iterator[ShellData], captureOutput: Boolean): ExecutionResult = {
    createProcessBuilder(name, args, env) match {
      case Some(pb) =>

        if (!captureOutput) {
          Terminal.restore()
          pb.inheritIO()
          val process = pb.start()
          process.waitFor()
          Terminal.setRaw()
          ExecutionResult(Iterator.empty, exitCode = process.exitValue())
        } else {

          val process = pb.start()

          if (stdin.nonEmpty) {
            val thread = new Thread(() => {
              Using(new java.io.BufferedWriter(new java.io.OutputStreamWriter(process.getOutputStream))) { writer =>
                stdin.foreach { data =>
                  writer.write(data.asString)
                  writer.newLine()
                }
                writer.flush()
              }
              ()
            })
            thread.setDaemon(true)
            thread.start()
          }

          val reader = new java.io.BufferedReader(new java.io.InputStreamReader(process.getInputStream))
          val outputIterator = new Iterator[ShellData] {
            private var nextLine: String = reader.readLine()
            
            override def hasNext: Boolean = nextLine != null
            
            override def next(): ShellData = {
              val current = nextLine
              nextLine = reader.readLine() // Forbered næste kald
              
              // Hvis vi er færdige, vent på at processen dør pænt
              if (nextLine == null) {
                process.waitFor() 
                reader.close()       
              }
              ShellData.Text(current)
            }
          }

          var errout = ""
          val errThread = new Thread(() => {
            errout = scala.io.Source.fromInputStream(process.getErrorStream).getLines().mkString("\n")
          })
          errThread.start()

          ExecutionResult(outputIterator, stderr = errout)
        }

      case None => 
        ExecutionResult(Iterator(ShellData.Text(s"$name: not found")), exitCode = 127)
    }
  }

  private def createProcessBuilder(name: String, args: List[String], env: ShellEnv): Option[java.lang.ProcessBuilder] = {
    Path.findInPath(name).map { fullPath =>
      val expandedArgs = args.map(arg => Files.expandPath(arg))
      val pb = new java.lang.ProcessBuilder((fullPath.toString :: expandedArgs)*)
      pb.directory(env.cwd.toFile)

      val pbEnv = pb.environment()
      env.variables.foreach { case (key, shellVar) =>
        if (shellVar.scope == VarScope.Global) {
          pbEnv.put(key, shellVar.value)
        }
      }
      pb
    }
  }

  private def handleType(cmd: String): String = {
    if (Builtin.isBuiltin(cmd)) s"$cmd is a shell builtin"
    else Path.findInPath(cmd).map(p => s"$cmd is $p").getOrElse(s"$cmd: not found")
  }

  private def handleCd(pathStr: String, currentCwd: JPath): Either[String, JPath] = {
    val target = Files.expandPath(pathStr)
    val p = Paths.get(target)

    val absP = if (p.isAbsolute) p else currentCwd.resolve(target)
    val normP = absP.normalize()

    if (JFiles.isDirectory(normP)) Right(normP)
    else Left(s"cd: $pathStr: No such file or directory")
  }
}
