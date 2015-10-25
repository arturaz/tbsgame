package implicits

import akka.event.Logging
import akka.typed.{ActorContext, ActorRef, ActorSystem}
import akka.{actor => untyped}

package object actor {
  object TypedActorSystemExts {
    private val asUntyped =
      classOf[ActorSystem[_]].getDeclaredMethod("untyped")
  }
  implicit class TypedActorSystemExts(val as: ActorSystem[_]) extends AnyVal {
    def asUntyped =
      TypedActorSystemExts.asUntyped.invoke(as).asInstanceOf[akka.actor.ActorSystem]
  }

  object TypedActorRefExts {
    private val asUntyped =
      classOf[ActorRef[_]].getDeclaredMethod("untypedRef")
  }
  implicit class TypedActorRefExts(val ref: ActorRef[_]) extends AnyVal {
    def asUntyped =
      TypedActorRefExts.asUntyped.invoke(ref).asInstanceOf[akka.actor.ActorRef]
  }

  implicit class TypedActorContextExts[A](val ctx: ActorContext[A]) extends AnyVal {
    def createLogging() = Logging(ctx.system.asUntyped, ctx.self.asUntyped)

    /** As `spawnAdapter` but gives access to the untyped ActorRef which sent
      * the original message. */
    def spawnAdapterUTRef[B](f: (B, untyped.ActorRef) => A) =
      ActorRef[B](ctx.actorOf(untyped.Props(new UTRefMessageWrapper(f))))

    /** Watch `ref` and send `msg` to self when it terminates. */
    def watchWith[B](ref: ActorRef[B], msg: A): ActorRef[B] = {
      watchWith(ref, msg, ctx.self)
      ref
    }

    /** Watch `ref` and send `msg` to `sendTo` when it terminates. */
    def watchWith[B, C](ref: ActorRef[B], msg: C, sendTo: ActorRef[C]): ActorRef[B] = {
      import akka.typed._
      import ScalaDSL._
      ctx.spawnAnonymous(Props(ContextAware[Unit] { anonCtx =>
        anonCtx.watch(ref)

        Full {
          case Sig(_, Terminated(`ref`)) =>
            sendTo ! msg
            Stopped
        }
      }))
      ref
    }
  }
}
