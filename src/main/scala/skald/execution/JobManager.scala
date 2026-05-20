package skald 

import scala.collection.mutable

object JobManager {

  case class BackgroundJob(
    id: Int,
    pid: Long,
    command: String,
    process: java.lang.Process,
    status: JobStatus = Running
  )

  val jobTable: mutable.Map[Int, BackgroundJob] = mutable.Map.empty
  var jobHistory: List[Int] = Nil

  def nextJobId: Int = 
    LazyList.from(1).find(id => !jobTable.contains(id)).get

  def addJob(job: BackgroundJob): Unit =
    jobTable(job.id) = job
    jobHistory = job.id :: jobHistory

  def removeJob(id: Int): Unit =
    jobTable.remove(id)
    jobHistory = jobHistory.filter(j => j != id)

  def printJob(job: BackgroundJob): Unit =
    val symbol = jobHistory match {
      case first :: _ if job.id == first => "+"
      case _ :: second :: _ if job.id == second => "-"
      case _ => " "
    }

    val jobId = s"[${job.id}]$symbol"

    val status = if (!job.process.isAlive) Done else Running
    val jobStatus = status.displayName.padTo(24, ' ')
    
    status match {
      case Running => System.out.println(s"$jobId  $jobStatus${job.command} &")
      case Done => System.out.println(s"$jobId  $jobStatus${job.command}")
    }

  def reapJobs(): Unit =
    val finishedJobs = jobTable.values.toList.sortBy(_.id).filter(!_.process.isAlive()).foreach { job => 
      printJob(job)
      removeJob(job.id)
    }
}
