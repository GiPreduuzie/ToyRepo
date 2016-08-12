using Contracts;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.ServiceModel;
using System.Threading;

namespace WCFServices
{
    class Program
    {
        static bool _serviceIsRunning = false;
        static ServiceHost _shipServiceHost = null;
        static ServiceHost _messagingServiceHost = null;

        public static List<string> Messages = new List<string>();

        static void Main(string[] args)
        {
            while (true)
            {
                Console.Clear();
                WriteMessages();

                if (_serviceIsRunning)
                    HandleRunningService();
                else
                    HandleLyingService();
                
                Console.WriteLine();
                Console.Write("..> ");

                var input = Console.ReadLine().Trim();

                switch (input)
                {
                    case "start": StartService(); break;
                    case "stop": StopService(); break;
                    case "send message": SendMessage(); break;
                    case "exit": Dispose(); return;
                }
            }
        }

        private static void WriteMessages()
        {
            Console.WriteLine($"Host process id: {Process.GetCurrentProcess().Id}, host thread id: {Thread.CurrentThread.ManagedThreadId}");
            Console.WriteLine(string.Join(Environment.NewLine, Messages));
            Console.WriteLine();
        }

        private static void SendMessage()
        {
            Console.WriteLine("What to say?");
            var message = Console.ReadLine();

            var factory = new ChannelFactory<IMessagingContract>("");
            factory.Open();
            factory.CreateChannel().GetMessage(message);
            factory.Close();
        }

        private static void Dispose()
        {
            if (_shipServiceHost != null 
                && _shipServiceHost.State == CommunicationState.Opened)
                _shipServiceHost.Close();

            if (_messagingServiceHost != null
               && _messagingServiceHost.State == CommunicationState.Opened)
                _messagingServiceHost.Close();
        }

        private static void StopService()
        {
            if (_serviceIsRunning)
            {
                _shipServiceHost.Close();
                _messagingServiceHost.Close();
                _serviceIsRunning = false;
            }
        }

        private static void StartService()
        {
            if (!_serviceIsRunning)
            {
                Console.WriteLine("Trying to start service...");

                try
                {
                    _shipServiceHost = new ServiceHost(typeof(ShipsContract));
                    _shipServiceHost.Open();

                    _shipServiceHost = new ServiceHost(typeof(MessagingService));
                    _shipServiceHost.Open();

                    _serviceIsRunning = true;
                }
                catch (AddressAccessDeniedException exc)
                {
                    Console.WriteLine("I am denied in adress registration!");
                    Console.WriteLine(exc.Message);
                    Console.ReadKey();
                }
                catch (Exception exc)
                {
                    Console.WriteLine(exc.Message);
                }
            }
        }

        static void HandleLyingService()
        {
            var itWas = Console.ForegroundColor;
            Console.ForegroundColor = ConsoleColor.Yellow;
            Console.WriteLine("Service is down");
            Console.ForegroundColor = itWas;

            Console.WriteLine("Type 'start' to run the service or 'exit' to finish");
        }

        static void HandleRunningService()
        {
            var itWas = Console.ForegroundColor;
            Console.ForegroundColor = ConsoleColor.Green;
            Console.WriteLine("Service is up and running");
            Console.ForegroundColor = itWas;

            Console.WriteLine("Type 'stop' to stop the service or 'exit' to finish");
        }
    }
}
