using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RandomChoiceMonad
{
    class Matrix
    {
        public Survey[] Surveys;
    }

    class Survey
    {
        public Question[] Questions;
    }

    class Question
    {
        public Field[] Fields;
    }

    class Field
    {
        public string Name;
    }



    class Program
    {
        static Matrix[] GetModel()
        {
            return new Matrix[]
            {
                new Matrix {
                    Surveys = new [] {
                        new Survey { Questions = new Question[0] },
                        new Survey {
                            Questions = new [] {
                                new Question { Fields = new Field[0]} } },
                        new Survey {
                            Questions = new [] {
                                new Question { Fields = new[] { new Field {Name = "1" }, new Field { Name = "1" } }},
                                new Question { Fields = new Field[0]} } },
                        new Survey { Questions = new Question[0] }
                } },
                new Matrix()
            };
        }


        static void Main(string[] args)
        {
            var random = new Random();

            for (int i = 0; i < 10; i++)
            {
                foreach (var matrix in GetModel())
                {
                    var result = RandomChoiceMonad.Return(matrix, random)
                        .Get(x => x.Surveys)
                        .Get(x => x.Questions)
                        .Get(x => x.Fields)
                        .Resolve();

                    Console.WriteLine(result?.Name ?? "< nothing >");
                }
            }
        }
    }
}