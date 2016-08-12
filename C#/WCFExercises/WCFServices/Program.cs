using Contracts;
using System;
using System.Collections.Generic;
using System.Linq;
using System.ServiceModel;
using System.Text;
using System.Threading.Tasks;

namespace WCFServices
{
    class Program
    {
        static void Main(string[] args)
        {
            var host = new ServiceHost(typeof(ShipsContract));
            host.Open();

            Console.WriteLine("Started and running!");
            Console.WriteLine("Press [Enter] to stop");
            Console.ReadKey();

            host.Close();
        }
    }
}
