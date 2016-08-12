using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Contracts;
using System.Linq;

namespace ServicesTests
{
    [TestClass]
    public class ServicesTests
    {
        [TestMethod]
        public void TestShipService()
        {
            var ships = new ShipsContract().GetShips();

            Assert.AreEqual(4, ships.Count());
        }
    }
}
