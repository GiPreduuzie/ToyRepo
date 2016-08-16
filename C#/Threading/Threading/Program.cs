using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace Threading
{
    class Program
    {
        static Task SleepAsync(int miliseconds)
        {
            TaskCompletionSource<bool> tcs = null;
            var timer = new Timer(delegate { tcs.TrySetResult(true); }, null, -1, -1);
            tcs = new TaskCompletionSource<bool>(timer);
            timer.Change(miliseconds, -1);
            return tcs.Task;
        }

        static int[] FindPrimeNumber(int n)
        {
            Console.WriteLine("prime numbers in : " + Thread.CurrentThread.ManagedThreadId);
            var primes = new List<int>() { };

            int i = 2;
            while (primes.Count != n)
            {
                if (primes.All(x => i % x != 0))
                    primes.Add(i);
                i++;
            }

            primes.Insert(0, 1);
            return primes.ToArray();
        }

        static Task<int[]> FindPrimeNumberAsync(int n)
        {
            return Task.Run(() => FindPrimeNumber(n));
        }

        static async void DoWork()
        {
            Console.WriteLine("do work in : " + Thread.CurrentThread.ManagedThreadId);
            var n = int.Parse(Console.ReadLine());

            Console.WriteLine("Starting work...");
            Console.WriteLine("That is what I do in between");

            var result = await FindPrimeNumberAsync(n);

            Console.WriteLine("after : " + Thread.CurrentThread.ManagedThreadId);
            Console.WriteLine("Ready");
            Console.WriteLine(result.Last());
        }

        static void Main(string[] args)
        {
            Console.WriteLine("main : " + Thread.CurrentThread.ManagedThreadId);
            DoWork();

            for (int i=0; i<100; i++)
            {
                Thread.Sleep(100);
                Console.Write("| " + i);
            }

            Console.WriteLine();
            Console.ReadKey();
        }
    }
}
