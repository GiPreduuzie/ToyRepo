using RandomChoiceMonad.Model;
using RandomChoiceMonad.RandomChoiceMonad;
using System;
using System.Text;

namespace RandomChoiceMonad
{
    class Program
    {
        static void Main(string[] args)
        {
            var random = new Random();

            for (int i = 0; i < 10; i++)
            {
                foreach (var input in new ModelRepository().GetTestModel())
                {
                    var itWas = Console.ForegroundColor;
                    Console.ForegroundColor = ConsoleColor.Yellow;
                    Console.WriteLine($"iteration {i}");
                    Console.ForegroundColor = itWas;


                    var logger = new StringBuilder();
                    var matrixM = ExhaustiveRandomChoiceMonad.Return(
                        input, 
                        CollectionModifiers.CollectionModifiers.CreateRandomCollectionModifier(random),
                        logger);

                    Console.WriteLine(logger.ToString());

                    var surveyM = matrixM.Get(x => x.Surveys);
                    var questionM = surveyM.Get(x => x.Questions);

                    var questionFixed = questionM.Resolve();

                    var fieldM = questionM.Get(x => x.Fields);

                    var matrix = matrixM.Resolve();
                    var survey = surveyM.Resolve();
                    var question = questionM.Resolve();
                    var field = fieldM.Resolve();

                    Output(matrix);
                    Output(survey);
                    Output(question);
                    Output(field);
                }
            }
        }

        static void Output<T>(T obj)
        {
            if (obj == null)
                Console.WriteLine("NULL");
            else
            {
                dynamic named = obj;
                Console.WriteLine("{0}: {1}", typeof(T).Name, named.Name ?? "<nothing>");
            }
        }
    }
}