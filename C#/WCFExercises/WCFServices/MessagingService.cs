using Contracts;
using System;
using System.Diagnostics;
using System.Text;
using System.Threading;

namespace WCFServices
{
    class MessagingService : IMessagingContract
    {
        public void GetMessage(string message)
        {
            var addition = $"Service process id: {Process.GetCurrentProcess().Id}, service thread id: {Thread.CurrentThread.ManagedThreadId}";
            Program.Messages.Add(new StringBuilder().AppendLine(addition).Append(message).ToString());
        }
    }
}
