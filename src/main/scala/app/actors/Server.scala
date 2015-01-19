package app.actors

import java.net.InetSocketAddress
import java.nio.ByteOrder

import akka.actor.{Props, Actor, ActorLogging, ActorRef}
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import app.persistence.DBDriver

class Server(
  port: Int, gamesManager: ActorRef, db: DBDriver.Database
)(implicit byteOrder: ByteOrder) extends Actor with ActorLogging {
  import context.system

  val manager = IO(Tcp)
  manager ! Bind(self, new InetSocketAddress(port))

  def receive = {
    case Bound(localAddress) =>
      log.info(s"Server bound to $localAddress")

    case CommandFailed(b: Bind) =>
      log.error(s"Cannot bind to ${b.localAddress}!")
      context.stop(self)

    case Connected(remote, local) =>
      log.info(s"Client connected from $remote.")
      implicit val logger = log
      val connection = sender()
      val msgHandler = context.actorOf(Props(new MsgHandler(
        connection,
        handlerRef => Props(new NetClient(handlerRef, gamesManager, db))
      )), s"${remote.getHostString}-${remote.getPort}")
      connection ! Register(msgHandler)
  }
}
