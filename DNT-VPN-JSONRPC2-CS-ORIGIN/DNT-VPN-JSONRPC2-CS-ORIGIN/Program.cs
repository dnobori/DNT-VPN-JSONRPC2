using System;
using SoftEther.JsonRpc;
using SoftEther.VPNServerRpc;

namespace DNT_VPN_JSONRPC
{
    class Program
    {
        static void Main(string[] args)
        {
            VPNRPCTest test = new VPNRPCTest();

            int i = 0;

            for (i = 0; ; i++)
            {
                Console.WriteLine("-------------");
                Console.WriteLine($"Test #{i} start");
                test.Test_All();
                Console.WriteLine($"Test #{i} finish");
                Console.WriteLine("-------------");
            }

            //Tools.GenCode1();

            //RpcServerInfo a = r.GetServerInfoAsync().Result;

            //a.Print();



            //VpnRpcSetUser a = new VpnRpcSetUser(){

        }
    }
}
