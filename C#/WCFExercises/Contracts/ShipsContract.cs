using System;
using System.Collections.Generic;
using Core.Repositories;
using System.Linq;

namespace Contracts
{
    public class ShipsContract : IShipsContract
    {
        private IShipRepository _shipsRepository = new HardcodeShipRepository(); 

        public IEnumerable<Ship> GetShips()
        {
            return _shipsRepository.GetShips().Select(ConvertShip);
        }

        public IEnumerable<Ship> GetShips(string name)
        {
            return _shipsRepository.GetShips(name).Select(ConvertShip);
        }

        public IEnumerable<Ship> GetShipsYoungerThan(DateTime date)
        {
            return _shipsRepository.GetShipsYoungerThan(date).Select(ConvertShip);
        }

        private Ship ConvertShip(Core.Models.Ship ship)
        {
            return new Ship { Name = ship.Name, Launched = ship.LaunchedDate };
        }
    }
}
