using RandomChoiceMonad.Model;
using RandomChoiceMonad.RandomChoiceMonad;
using System;

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



                    var matrixM = ExhaustiveRandomChoiceMonad.Return(input, CollectionModifiers.CollectionModifiers.CreateRandomCollectionModifier(random));
                    var surveyM = matrixM.Get(x => x.Surveys);
                    var questionM = surveyM.Get(x => x.Questions);

                    var questionFixed = questionM.Resolve();

                    var fieldM = questionM.Get(x => x.Fields);

                    var matrix = matrixM.Resolve();
                    var survey = surveyM.Resolve();
                    var question = questionM.Resolve();
                    var field = fieldM.Resolve();

                    Console.WriteLine("matrix: " + (matrix?.Name ?? "< nothing >"));
                    Console.WriteLine("survey: " + (survey?.Name ?? "< nothing >"));
                    Console.WriteLine("question: " + (question?.Name ?? "< nothing >"));

                    Console.WriteLine("field: " + (field?.Name ?? "< nothing >"));

                    Console.WriteLine("(fixed question: " + (questionFixed?.Name ?? "< nothing >") + ")");
                    Console.WriteLine();
                }
            }
        }
    }
}