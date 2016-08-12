using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RandomChoiceMonad
{
    class Matrix
    {
        public string Name;
        public Survey[] Surveys;
    }

    class Survey
    {
        public string Name;
        public Question[] Questions;
    }

    class Question
    {
        public string Name;
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
                    Name = "1",
                    Surveys = new [] {
                        new Survey { Name = "1", Questions = new Question[0] },
                        new Survey { Name = "2",
                            Questions = new [] {
                                new Question { Name = "1*", Fields = new Field[0]} } },
                        new Survey { Name = "3",
                            Questions = new [] {
                                new Question { Name = "1", Fields = new[] { new Field {Name = "1" }, new Field { Name = "1" } }},
                                new Question { Name = "2", Fields = new Field[0]} } },
                        new Survey { Name = "4", Questions = new Question[0] }
                } },
               // new Matrix()
            };
        }


        static void Main(string[] args)
        {
            var random = new Random();

            for (int i = 0; i < 10; i++)
            {
                foreach (var input in GetModel())
                {
                    var matrixM = ExhaustiveRandomChoiceMonad.Return(input, random);
                    var surveyM = matrixM.Get(x => x.Surveys);
                    var questionM = surveyM.Get(x => x.Questions);

                    var questionFixed = questionM.Resolve();

                    var fieldM = questionM.Get(x => x.Fields);

                    var matrix = matrixM.Resolve();
                    var survey = surveyM.Resolve();
                    var question = questionM.Resolve();
                    var field = fieldM.Resolve();

                    Console.WriteLine("matrix: " + matrix?.Name ?? "< nothing >");
                    Console.WriteLine("survey: " + survey?.Name ?? "< nothing >");
                    Console.WriteLine("question: " + question?.Name ?? "< nothing >");

                    Console.WriteLine("field: " + field?.Name ?? "< nothing >");

                    Console.WriteLine("(fixed question: " + questionFixed?.Name ?? "< nothing >" + ")");
                }
            }
        }
    }
}