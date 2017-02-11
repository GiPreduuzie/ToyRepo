using System;
using TreeBuilder.Builders;

namespace TreeBuilder
{
    internal static class Program
    {
        private static void Main(string[] args)
        {
            var root =
                 new RootBuilder()
                     .AddFolder("First folder")
                         .AddFolder("Embedded folder")
                             .AddItem("1").Done()
                             .AddItem("2").WithDescription("second item in embedded folder").Done()
                             .Done()
                         .Done()
                     .AddFolder("Second folder")
                         .AddItem("1").WithDescription("some item").Done()
                         .AddItem("2").Done()
                         .Done()
                     .Done();

            Console.WriteLine(root);

            //That is an output:

            //[root:
            //     [folder: First folder
            //          [folder: Embedded folder
            //               [item: 1, <no description>]
            //               [item: 2, second item in embedded folder]
            //          ]
            //     ]
            //     [folder: Second folder
            //          [item: 1, some item]
            //          [item: 2, <no description>]
            //     ]
            //]
        }
    }
}
