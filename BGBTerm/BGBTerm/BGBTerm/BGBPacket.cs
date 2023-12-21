using System;
using System.Collections.Generic;
using System.Text;

namespace BGBTerm
{
    public enum BGBLinkCommand : byte
    {
        unknown = 0,
        joypad = 101,
        sync1 = 104,
        sync2 = 105,
        sync3 = 106,
        status = 108,
        wantdisconnect = 109
    }
    
    public class BGBPacket
    {
        public static byte Sync1ControlBits = 0b10000001;
        public static byte Sync1ControlDoubleSpeedBit = (1 << 2);
        public static byte Sync1ControlHighSpeedBit = (1 << 1);

        static System.Diagnostics.Stopwatch Watch = new System.Diagnostics.Stopwatch();
        static int TimeStampMhz = 2097152;
        public static UInt32 GetTimeStamp()
        {
            return (UInt32)(Watch.Elapsed.TotalSeconds * (double)TimeStampMhz);
        }
        public static void Init()
        {
            Watch.Start();
        }
        public BGBPacket(byte[] bytes)
        {
            if (bytes.Length != 8)
            {
                throw new InvalidOperationException($"BGBPacket CTOR: Invalid length passed: {bytes.Length}");
            }
            b1 = bytes[0];
            b2 = bytes[1];
            b3 = bytes[2];
            b4 = bytes[3];
            Timestamp = 0;
            Timestamp |= bytes[4];
            Timestamp |= (UInt32)(bytes[5] << 8);
            Timestamp |= (UInt32)(bytes[6] << 16);
            Timestamp |= (UInt32)(bytes[7] << 24);
        }
        public BGBPacket(byte[] bytes, UInt32 timestamp)
        {
            b1 = bytes[0];
            b2 = bytes[1];
            b3 = bytes[2];
            b4 = bytes[3];
            Timestamp = timestamp;
        }
        private BGBPacket()
        {
            b1 = 0;
            b2 = 0;
            b3 = 0;
            b4 = 0;
            Timestamp = 0;
        }

        public byte[] Bytes
        {
            get
            {
                return new Byte[8]
                {
                    b1,b2,b3,b4,(byte)(Timestamp&0xff), (byte)((Timestamp >> 8) & 0xff), (byte)((Timestamp >> 16) & 0xff), (byte)((Timestamp >> 24) & 0xff)
                };
            }
        }

        public BGBLinkCommand PacketType
        {
            get
            {
                switch((BGBLinkCommand)b1)
                {
                    case BGBLinkCommand.joypad:
                    case BGBLinkCommand.status:
                    case BGBLinkCommand.sync1:
                    case BGBLinkCommand.sync2:
                    case BGBLinkCommand.sync3:
                    case BGBLinkCommand.wantdisconnect:
                        return (BGBLinkCommand)b1;
                    default:
                        return BGBLinkCommand.unknown;

                }
            }
        }

        public bool IsVersionValid()
        {
            return RecievedVersionMessageIsCorrect(Bytes);
        }

        private static readonly byte[] VersionMessage = new byte[8] {
            1,1,4,0,0,0,0,0
        };

        private static bool RecievedVersionMessageIsCorrect(byte[] msg)
        {
            if (msg.Length != 8)
            {
                Console.WriteLine($"Incorrect length message recieved. got: {msg.Length} expected: 8");
                return false;
            }
            for (int i = 0; i < 8; i++)
            {
                if (msg[i] != VersionMessage[i])
                {
                    Console.WriteLine("Incorrect verstion message recieved.");
                    return false;
                }
            }
            return true;

        }
        public static BGBPacket VersionPacket()
        {
            return new BGBPacket(VersionMessage)
                .WithCurrentTimeStamp();
        }

        public static BGBPacket Sync1Packet(byte controlData, byte data)
        {
            return new BGBPacket()
                .WithCommandValue(BGBLinkCommand.sync1)
                .WithByteSet(1, data)
                .WithByteSet(2, controlData)
                .WithCurrentTimeStamp();
        }

        public static BGBPacket Sync2Packet(byte controlData, byte data)
        {
            return new BGBPacket()
                .WithCommandValue(BGBLinkCommand.sync2)
                .WithByteSet(1, data)
                .WithByteSet(2, controlData)
                .WithCurrentTimeStamp();
        }

        public static BGBPacket Sync3Packet(byte bVal)
        {
            return new BGBPacket()
                .WithCommandValue(BGBLinkCommand.sync3)
                .WithByteSet(1, bVal)
                .WithCurrentTimeStamp();
        }

        public static BGBPacket StatusPacket()
        {
            return new BGBPacket()
                .WithCommandValue(BGBLinkCommand.status)
                .WithByteSet(1, 5)
                .WithCurrentTimeStamp();
        }

        public byte b1, b2, b3, b4;
        public UInt32 Timestamp;
    }

    public static class PacketExtensions
    {
        public static BGBPacket WithTimeStamp(this BGBPacket packet, UInt32 timeStamp)
        {
            packet.Timestamp = timeStamp;
            return packet;
        }
        public static BGBPacket WithCurrentTimeStamp(this BGBPacket packet)
        {
            packet.Timestamp = BGBPacket.GetTimeStamp();
            return packet;
        }
        public static BGBPacket WithCommandValue(this BGBPacket packet, BGBLinkCommand cmd)
        {
            packet.b1 = (byte)cmd;
            return packet;
        }
        public static BGBPacket WithByteSet(this BGBPacket packet, int num, byte val)
        {
            switch(num)
            {
                case 0:
                    packet.b1 = val;
                    break;
                case 1:
                    packet.b2 = val;
                    break;
                case 2:
                    packet.b3 = val;
                    break;
                case 3:
                    packet.b4 = val;
                    break;
            }
            return packet;
        }
    }
}
