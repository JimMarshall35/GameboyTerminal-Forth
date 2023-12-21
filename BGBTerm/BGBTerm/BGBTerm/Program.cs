using System;
using System.Net;
using System.Net.Sockets;
using System.Threading.Tasks;
using System.Collections.Concurrent;
using System.Threading;

namespace BGBTerm
{

    class Program
    {
        static Socket listener;
        static ConcurrentQueue<BGBPacket> MessagesToSend = new ConcurrentQueue<BGBPacket>();
        static bool bShouldStop = false;
        static void Main(string[] args)
        {

            BGBPacket.Init();

            Task hs = Task.Run(ClientTask);

            byte[] buf = new byte[8];
            while (true)
            {
                ConsoleKeyInfo info = Console.ReadKey();
                //Console.WriteLine(info.KeyChar);
                byte key = (byte)info.KeyChar;
                if(key == 0xd)
                {
                    key = 0x0a;
                }
                MessagesToSend.Enqueue(BGBPacket.Sync1Packet(BGBPacket.Sync1ControlBits, key));
            }
            bShouldStop = true;
            hs.Wait();

        }

        static void PrintPacket(byte[] packet, bool sendOrRecieve)
        {
            Console.WriteLine(sendOrRecieve ? "Sending Packet:" : "Recieving packet:");
           
            string hexString = BitConverter.ToString(packet);
            Console.WriteLine(hexString);
            var cmd = (BGBLinkCommand)packet[0];
            Console.WriteLine($"command type: {cmd.ToString()}");
            switch (cmd)
            {
                case BGBLinkCommand.sync1:
                case BGBLinkCommand.sync2:
                    Console.WriteLine($"Data: {packet[1].ToString("X").PadLeft(2, '0')}");
                    Console.WriteLine($"Control Data: {packet[2].ToString("X").PadLeft(2, '0')}");
                    break;
                case BGBLinkCommand.status:
                    Console.WriteLine($"Running: {((packet[1] & 1) > 0 ? "true" : "false")}");
                    Console.WriteLine($"Paused: {((packet[1] & (1 << 1)) > 0 ? "true" : "false")}");
                    Console.WriteLine($"Supportreconnect: {((packet[1] & (1 << 2)) > 0 ? "true" : "false")}");
                    break;

            }
            UInt32 ts = 0;
            ts |= packet[4];
            ts |= (UInt32)(packet[5] << 8);
            ts |= (UInt32)(packet[6] << 16);
            ts |= (UInt32)(packet[7] << 24);
            Console.WriteLine($"TimeStamp: {ts}");
            Console.WriteLine();
        }

        static void ClientTask()
        {
            
            string hostName = "127.0.0.1";
            int hostPort = 5678;
            int response = 0;

            IPAddress host = IPAddress.Parse(hostName);
            IPEndPoint hostep = new IPEndPoint(host, hostPort);
            Socket sock = new Socket(hostep.AddressFamily, SocketType.Stream, ProtocolType.Tcp);

            while (true)
            {
                try
                {
                    sock.Connect(hostep);
                    break;
                }
                catch(Exception e)
                {
                    Console.WriteLine("Failed to connect, retrying in 3 seconds");
                    Thread.Sleep(3000);
                }

            }
            
            PrintPacket(BGBPacket.VersionPacket().Bytes, true);
            response = sock.Send(BGBPacket.VersionPacket().Bytes);
            Console.WriteLine(response);
            byte[] rBuf = new byte[8];
            int num = sock.Receive(rBuf);
            PrintPacket(rBuf, false);
            if(rBuf[0] == 1 && rBuf[1] == 1 && rBuf[2] == 4)
            {
                Console.WriteLine("Correct emulator version");
                PrintPacket(BGBPacket.StatusPacket().Bytes, true);
                response = sock.Send(BGBPacket.StatusPacket().Bytes);
                num = sock.Receive(rBuf);
                PrintPacket(rBuf, false);
            }
            else
            {
                Console.WriteLine("Invalid emulator version");
                return;
            }

            while (true)
            {
                while(MessagesToSend.TryDequeue(out BGBPacket packet))
                {
                    PrintPacket(packet.Bytes, true);
                    response = sock.Send(packet.Bytes);
                    num = sock.Receive(rBuf);
                    PrintPacket(rBuf, false);
                    BGBPacket responsePacket = new BGBPacket(rBuf);
                    switch(responsePacket.PacketType)
                    {
                        case BGBLinkCommand.sync3:
                            PrintPacket(BGBPacket.Sync3Packet(0).Bytes, true);
                            sock.Send(BGBPacket.Sync3Packet(0).Bytes);
                            break;
                        case BGBLinkCommand.status:
                            PrintPacket(BGBPacket.StatusPacket().Bytes, true);
                            sock.Send(BGBPacket.StatusPacket().Bytes);
                            break;

                    }
                }
                // send a constant stream of sync3 pack
                sock.Send(BGBPacket.Sync3Packet(0).Bytes);
                Thread.Sleep(30);
            }
        }


        //    static async Task InitialHandshake()
        //    {
        //        Console.WriteLine("Waiting for connection from bgb!");
        //        IPAddress ipAddress = IPAddress.Parse("127.0.0.1");
        //        IPEndPoint ipEndPoint = new IPEndPoint(ipAddress, 5678);

        //        listener = new Socket(
        //        ipEndPoint.AddressFamily,
        //        SocketType.Stream,
        //        ProtocolType.Tcp);

        //        listener.Bind(ipEndPoint);
        //        listener.Listen(100);

        //        var handler = await listener.AcceptAsync();

        //        // Receive message.
        //        var buffer = new byte[8];
        //        var received = await handler.ReceiveAsync(buffer, SocketFlags.None);
        //        Console.WriteLine("Recieved Packet:");
        //        for (int i = 0; i < received; i++)
        //        {
        //            Console.Write(buffer[i]);
        //        }
        //        Console.Write("\n");
        //        BGBPacket packet = new BGBPacket(buffer);
        //        if (packet.IsVersionValid())
        //        {
        //            await handler.SendAsync(buffer, 0);
        //        }
        //        Console.WriteLine("Connection Successful");
        //        while(!bShouldStop)
        //        {
        //            if(!MessagesToSend.IsEmpty)
        //            {
        //                while (MessagesToSend.TryDequeue(out BGBPacket result))
        //                {
        //                    await handler.SendAsync(result.Bytes, 0);
        //                    //int recieved = listener.Receive(buffer);
        //                    //Console.WriteLine("Recieved Packet:");
        //                    //for (int i = 0; i < recieved; i++)
        //                    //{
        //                    //    Console.Write(buffer[i]);
        //                    //}
        //                    //Console.Write("\n");
        //                }
        //            }
        //        }
        //    }
        //}
    }
}
