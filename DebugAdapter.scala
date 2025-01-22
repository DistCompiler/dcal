// Copyright 2025 DCal Team
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package distcompiler

import java.nio.channels.ServerSocketChannel
import java.net.InetSocketAddress
import java.nio.channels.SocketChannel
import java.nio.channels.ClosedChannelException
import java.nio.channels.AsynchronousCloseException
import java.util.concurrent.LinkedBlockingQueue

import scala.collection.mutable
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.CharBuffer
import java.nio.channels.Channels
import java.io.Closeable

final class DebugAdapter(host: String, port: Int) extends Manip.Tracer:
  private val io = DebugAdapter.IO(host, port)

  @scala.annotation.tailrec
  private def handlePointOfInterest(
      debugInfo: DebugInfo,
      refMap: Manip.RefMap
  ): Unit =
    val shouldWait =
      io.withState: state =>
        if !state.isPaused
        then
          state.debugInfo = null
          state.refMap = null
          false
        else
          println(s"pausing...")
          state.debugInfo = debugInfo
          state.refMap = refMap
          true

    if shouldWait
    then
      io.awaitStateUpdate()
      println("wake up...")
      handlePointOfInterest(debugInfo, refMap)

  def close(): Unit =
    io.close()

  def beforePass(debugInfo: DebugInfo)(using Manip.RefMap): Unit =
    handlePointOfInterest(debugInfo, summon[Manip.RefMap])

  def afterPass(debugInfo: DebugInfo)(using Manip.RefMap): Unit =
    handlePointOfInterest(debugInfo, summon[Manip.RefMap])

  def onRewriteMatch(
      debugInfo: DebugInfo,
      parent: Node.Parent,
      idx: Int,
      matchedCount: Int
  )(using Manip.RefMap): Unit =
    handlePointOfInterest(debugInfo, summon[Manip.RefMap])

  def onRewriteComplete(
      debugInfo: DebugInfo,
      parent: Node.Parent,
      idx: Int,
      resultCount: Int
  )(using Manip.RefMap): Unit =
    handlePointOfInterest(debugInfo, summon[Manip.RefMap])

  def onCommit(debugInfo: DebugInfo)(using Manip.RefMap): Unit =
    handlePointOfInterest(debugInfo, summon[Manip.RefMap])
  def onBacktrack(debugInfo: DebugInfo)(using Manip.RefMap): Unit =
    handlePointOfInterest(debugInfo, summon[Manip.RefMap])

  def onFatal(debugInfo: DebugInfo, from: DebugInfo)(using Manip.RefMap): Unit =
    handlePointOfInterest(debugInfo, summon[Manip.RefMap])

object DebugAdapter:
  final class State:
    var isPaused = true

    var debugInfo: DebugInfo | Null = null
    var refMap: Manip.RefMap | Null = null

  object DAPProtocolMessage:
    def unapply(js: ujson.Value): Option[(Int, String, ujson.Obj)] =
      js match
        case obj: ujson.Obj
            if obj("seq").numOpt
              .exists(_.isValidInt) && obj("type").strOpt.nonEmpty =>
          Some((obj("seq").num.toInt, obj("type").str, obj))
        case _ => None

  object DAPRequest:
    def unapply(js: ujson.Value): Option[(Int, String, ujson.Value)] =
      js match
        case DAPProtocolMessage(seq, "request", obj)
            if obj("command").strOpt.nonEmpty =>
          Some(
            (
              seq,
              obj("command").str,
              obj.obj.getOrElse("arguments", ujson.Obj())
            )
          )
        case _ => None

  final class IO(host: String, port: Int) extends java.io.Closeable:
    io =>

    private final class Connection(sock: SocketChannel)
        extends Runnable,
          Closeable:
      conn =>
      private val input = Channels.newInputStream(sock)
      private val output = Channels.newOutputStream(sock)
      private var responseIdx = 0
      private val thread = Thread(this, "debug-adapter-connection")
      thread.start()

      private object state:
        private val headerParts = mutable.ListBuffer.empty[String]
        private val headerPattern = raw"""Content-Length: (\d+)""".r
        private val builder = StringBuilder()

        def onChar(char: Char): Unit =
          (builder.lastOption, char) match
            case (Some('\r'), '\n') =>
              builder.deleteCharAt(builder.size - 1)
              val part = builder.result()
              builder.clear()
              if part == ""
              then onHeader()
              else headerParts += part
            case (_, ch) => builder += ch

        def onHeader(): Unit =
          var contentLength = -1
          headerParts.foreach:
            case headerPattern(contentLengthStr) =>
              contentLength = contentLengthStr.toInt
            case part =>
              throw AssertionError(s"unrecognized DAP header: $part")

          assert(contentLength != -1, "content length missing from DAP header")

          val bytes = input.readNBytes(contentLength)
          handleMsg(ujson.read(bytes))

      def run(): Unit =
        def closedConnection(): Unit =
          connectionOpt = None
          println(s"closed connection")
        while true
        do
          try
            val result = input.read()
            if result == -1
            then
              sock.close()
              closedConnection()
              return

            state.onChar(result.toChar)
          catch
            case _: (ClosedChannelException | AsynchronousCloseException) =>
              // we are closing, goodbye
              closedConnection()
              return

      private def sendMsg(payload: ujson.Obj): Unit =
        // synchronized, because close() might call us to send a terminate event
        conn.synchronized:
          payload("seq") = responseIdx
          responseIdx += 1
          val payloadBytes = ujson.writeToByteArray(payload)
          output.write(
            s"Content-Length: ${payloadBytes.length}\r\n\r\n"
              .getBytes(StandardCharsets.UTF_8)
          )
          output.write(payloadBytes)

      private def sendResp(
          requestSeq: Int,
          command: String,
          body: ujson.Value = ujson.Null
      ): Unit =
        sendMsg(
          ujson.Obj(
            "request_seq" -> requestSeq,
            "success" -> true,
            "command" -> command,
            "type" -> "response",
            "body" -> body
          )
        )

      private def sendEvent(
          event: String,
          body: ujson.Value = ujson.Null
      ): Unit =
        sendMsg(
          ujson.Obj(
            "type" -> "event",
            "event" -> event,
            "body" -> body
          )
        )

      private def handleMsg(msg: ujson.Value): Unit =
        msg match
          case DAPRequest(seq, "initialize", req) =>
            println(s"initialize req: $req")
            sendResp(
              requestSeq = seq,
              command = "initialize"
            )
          case DAPRequest(seq, "attach", req) =>
            println(s"attach req: $req")
            sendResp(
              requestSeq = seq,
              command = "attach"
            )
            sendEvent(
              event = "stopped",
              body = ujson.Obj(
                "reason" -> "pause",
                "description" -> "Ready to start",
                "threadId" -> 1
              )
            )
          case DAPRequest(seq, "disconnect", req) =>
            println(s"disconnect req: $req")
            sendResp(
              requestSeq = seq,
              command = "disconnect"
            )
            sock.close()
          case DAPRequest(seq, "threads", req) =>
            println(s"threads req: $req")
            sendResp(
              requestSeq = seq,
              command = "threads",
              body = ujson.Obj(
                "threads" -> ujson.Arr(
                  ujson.Obj(
                    "id" -> 1,
                    "name" -> "singleton",
                    "threadId" -> 1
                  )
                )
              )
            )
          case DAPRequest(seq, "pause", req) =>
            println(s"pause req: $req")
            val wasPaused =
              io.withState: state =>
                val wasPaused = state.isPaused
                state.isPaused = true
                wasPaused
            sendResp(
              requestSeq = seq,
              command = "pause"
            )
            if wasPaused
            then
              sendEvent(
                event = "stopped",
                body = ujson.Obj(
                  "reason" -> "pause",
                  "description" -> "Paused",
                  "theadId" -> 1
                )
              )
          case DAPRequest(seq, "stackTrace", req) =>
            println(s"stackTrace req: $req")
            withState: state =>
              if state.isPaused && (state.debugInfo ne null)
              then
                val info = state.debugInfo.nn
                val rec1 = info.records.head
                sendResp(
                  requestSeq = seq,
                  command = "stackTrace",
                  body = ujson.Obj(
                    "stackFrames" -> ujson.Arr(
                      ujson.Obj(
                        "id" -> 1,
                        "name" -> rec1.fileName,
                        "path" -> rec1.file,
                        "line" -> rec1.line,
                        "column" -> 0,
                        "origin" -> "manip"
                      )
                    ),
                    "totalFrames" -> 1
                  )
                )
          // TODO: LoadedSourceEvent probably removes the "unknown source" label
          case DAPRequest(seq, "continue", req) =>
            println(s"continue req: $req")
            io.withState: state =>
              state.isPaused = false
            sendResp(
              requestSeq = seq,
              command = "continue"
            )
          case _ =>
            println(s"unsupported DAP request ${msg.render()}")

      def close(): Unit =
        sendEvent(
          event = "terminated"
        )
        sock.close()
        thread.join()

    private var connectionOpt: Option[Connection] = None

    private val _state = State()

    def withState[T](fn: State => T): T =
      io.synchronized:
        val result = fn(_state)
        io.notifyAll()
        result

    def awaitStateUpdate(): Unit =
      io.synchronized(io.wait())

    private final class ConnectionAccepter extends Runnable, Closeable:
      private val serverChannel =
        ServerSocketChannel
          .open()
          .setOption(java.net.StandardSocketOptions.SO_REUSEADDR, true)
          .bind(InetSocketAddress(host, port))
      private val thread = Thread(this, "debug-adapter-listener")
      thread.start()

      def run(): Unit =
        println(s"listening for DAP on ${serverChannel.getLocalAddress()}")
        while true
        do
          try
            val sock = serverChannel.accept()
            println(s"received connection from ${sock.getRemoteAddress()}")
            io.synchronized:
              connectionOpt match
                case None =>
                  connectionOpt = Some(Connection(sock))
                case Some(connection) =>
                  connectionOpt = None
                  connection.close()
                  sock.close()
          catch
            case _: (ClosedChannelException | AsynchronousCloseException) =>
              // means we are closing
              return

      def close(): Unit =
        serverChannel.close()
        thread.join()

    private val connectionAccepter = ConnectionAccepter()

    def close(): Unit =
      io.synchronized(connectionAccepter.close())
      io.synchronized:
        connectionOpt match
          case None =>
          case Some(connection) =>
            connection.close()
            connectionOpt = None
