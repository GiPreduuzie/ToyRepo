using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using RandomChoiceMonad.Model;
using RandomChoiceMonad.RandomChoiceMonad;
using RandomChoiceMonad.CollectionModifiers;
using System.Linq;

namespace ChoiceMonadTests
{
    [TestClass]
    public class ChoiceMonadTests
    {
        [TestMethod]
        public void GeneralTest()
        {
            var testModel = new ModelRepository().GetTestModel();
            var collectionModifiers = CollectionModifiers.CreateIdentityCollectionModifier();

            var input = new ModelRepository().GetTestModel().First();

            // 1
            var matrixM = ExhaustiveRandomChoiceMonad.Return(input, collectionModifiers);
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
