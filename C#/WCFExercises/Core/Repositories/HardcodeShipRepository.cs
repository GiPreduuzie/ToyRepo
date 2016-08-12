using Core.Models;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Core.Repositories
{
    public class HardcodeShipRepository : IShipRepository
    {
        public IEnumerable<Ship> GetShips()
        {
            return new[] {
                new Ship() {Name="Brave", ShipType = ShipTypes.Ocean, LaunchedDate = new DateTime(1958, 12, 1)},
                new Ship() {Name="SeaBeloved", ShipType = ShipTypes.Sea, LaunchedDate = new DateTime(1995, 2, 28)},
                new Ship() {Name="Vodyanoy", ShipType = ShipTypes.River, LaunchedDate = new DateTime(1912, 10, 5)},
                new Ship() {Name="Guest", ShipType = ShipTypes.River, LaunchedDate = new DateTime(2005, 6, 3) } };
        } 

        public IEnumerable<Ship> GetShips(string name)
        {
            return GetShips().Where(x => x.Name == name);
        }

        public IEnumerable<Ship> GetShipsYoungerThan(DateTime date)
        {
            return GetShips().Where(x => x.LaunchedDate > date);
        }
    }
}