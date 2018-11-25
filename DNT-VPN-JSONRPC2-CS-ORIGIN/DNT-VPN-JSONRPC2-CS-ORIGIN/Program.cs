using System;
using DNT_VPN_JSONRPC2_CS_ORIGIN.CodeGen;
using System.IO;
using System.Diagnostics;
using Newtonsoft.Json;
using SoftEther.VPNServerRpc;
using System.Text;
using SoftEther.JsonRpc;


namespace DNT_VPN_JSONRPC
{
    class Program
    {
        static void Main(string[] args)
        {
            //Console.WriteLine(CodeGenUtil.ProjectDir);

            CodeGen g = new CodeGen();

            g.Test();

            //VPNRPCTest test = new VPNRPCTest();

            //int i = 0;

            //for (i = 0; ; i++)
            //{
            //    Console.WriteLine("-------------");
            //    Console.WriteLine($"Test #{i} start");
            //    test.Test_All();
            //    Console.WriteLine($"Test #{i} finish");
            //    Console.WriteLine("-------------");
            //}

            //Tools.GenCode1();

            //RpcServerInfo a = r.GetServerInfoAsync().Result;

            //a.Print();



            //VpnRpcSetUser a = new VpnRpcSetUser(){

        }
    }
}






