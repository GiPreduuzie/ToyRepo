using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using RandomChoiceMonad.Model;
using RandomChoiceMonad.RandomChoiceMonad;
using RandomChoiceMonad.CollectionModifiers;
using System.Linq;
using System.Text;

namespace ChoiceMonadTests
{
    [TestClass]
    public class ChoiceMonadTests
    {
        StringBuilder _logger;

        private IChoiceMonad<T> MakeMonad<T>(T value) where T : class
        {
            _logger = new StringBuilder();
            var collectionModifiers = CollectionModifiers.CreateIdentityCollectionModifier();
            var matrixM = ExhaustiveRandomChoiceMonad.Return(value, collectionModifiers, _logger);

            return matrixM;
        }

        [TestMethod]
        public void NoSecondLevel()
        {
            var input = new Matrix { Name = "A", Surveys = new Survey[0] };

            var matrixM = MakeMonad(input);
            var surveyM = matrixM.Get(x => x.Surveys);

            Assert.AreEqual(null, matrixM.Resolve());
            Assert.AreEqual(null, surveyM.Resolve());
        }

        [TestMethod]
        public void SecondLevelIsNull()
        {
            var input = new Matrix { Name = "A", Surveys = null };

            var matrixM = MakeMonad(input);
            var surveyM = matrixM.Get(x => x.Surveys);

            Assert.AreEqual(null, matrixM.Resolve());
            Assert.AreEqual(null, surveyM.Resolve());
        }

        [TestMethod]
        public void SecondLevelAtRightEnd()
        {
            var input = new Matrix { Name = "A", Surveys = new[] { null, new Survey { Name = "C"} } };

            var matrixM = MakeMonad(input);
            var surveyM = matrixM.Get(x => x.Surveys);

            Assert.AreEqual("A", matrixM.Resolve().Name);
            Assert.AreEqual("C", surveyM.Resolve().Name);
        }

        [TestMethod]
        public void ThirdLevelAtRightEnd()
        {
            var input = new Matrix
            {
                Name = "A",
                Surveys = new[] {
                    new Survey { Name = "B" },
                    new Survey { Name = "C", Questions = new [] { new Question() {Name = "1" } } } }
            };

            var matrixM = MakeMonad(input);
            var surveyM = matrixM.Get(x => x.Surveys);
            var questionM = surveyM.Get(x => x.Questions);

            Assert.AreEqual("A", matrixM.Resolve().Name);
            Assert.AreEqual("C", surveyM.Resolve().Name);
            Assert.AreEqual("1", questionM.Resolve().Name);
        }

        [TestMethod]
        public void FailingCase()
        {
            var input = new Matrix
            {
                Name = "1",
                Surveys = new[] {
                        new Survey { Name = "1",
                            Questions = new [] {
                                new Question { Name = "1*", Fields = new Field[0]} } },
                        new Survey { Name = "2",
                            Questions = new [] {
                                new Question { Name = "1", Fields = new Field[0]},
                                new Question { Name = "2", Fields = new[] { new Field {Name = "1" }} } }

                }
            }
            };

            var matrixM = MakeMonad(input);
            var surveyM = matrixM.Get(x => x.Surveys);
            var questionM = surveyM.Get(x => x.Questions);
            var fieldM = questionM.Get(x => x.Fields);

            Assert.AreEqual("1", matrixM.Resolve().Name);
            Assert.AreEqual("2", surveyM.Resolve().Name);
            Assert.AreEqual("2", questionM.Resolve().Name);
            Assert.AreEqual("1", fieldM.Resolve().Name);
        }

        [TestMethod]
        public void GeneralTest()
        {
            var testModel = new ModelRepository().GetTestModel();
            var collectionModifiers = CollectionModifiers.CreateIdentityCollectionModifier();

            var input = new ModelRepository().GetTestModel().First();

            // 1
            var matrixM = ExhaustiveRandomChoiceMonad.Return(input, collectionModifiers, new StringBuilder());
            var matrix1 = matrixM.Resolve();
            
            // 2
            var surveyM = matrixM.Get(x => x.Surveys);
            var matrix2 = matrixM.Resolve();
            var survey2 = surveyM.Resolve();

            // 3
            var questionM = surveyM.Get(x => x.Questions);
            var matrix3 = matrixM.Resolve();
            var survey3 = surveyM.Resolve();
            var question3 = questionM.Resolve();

            // 4
            var fieldM = questionM.Get(x => x.Fields);
            var matrix4 = matrixM.Resolve();
            var survey4 = surveyM.Resolve();
            var question4 = questionM.Resolve();
            var field4 = fieldM.Resolve();

            // 1
            Assert.AreEqual("1", matrix1.Name);

            // 2
            Assert.AreEqual("1", matrix2.Name);
            Assert.AreEqual("1", survey2.Name);

            // 3
            Assert.AreEqual("1", matrix3.Name);
            Assert.AreEqual("2", survey3.Name);
            Assert.AreEqual("1*", question3.Name);

            // 4
            Assert.AreEqual("1", matrix4.Name);
            Assert.AreEqual("3", survey4.Name);
            Assert.AreEqual("1", question4.Name);
            Assert.AreEqual("1", field4.Name);
        }
    }
}
