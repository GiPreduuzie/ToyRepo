using Contracts;
using System.ServiceModel;
using System;
using System.Collections.Generic;

namespace Proxies
{
    public class ShipsContractClient : ClientBase<IShipsContract>, IShipsContract
    {
        public IEnumerable<Ship> GetShips()
        {
            return Channel.GetShips();
        }

        public IEnumerable<Ship> GetShips(string name)
        {
            return Channel.GetShips(name);
        }

        public IEnumerable<Ship> GetShipsYoungerThan(DateTime date)
        {
            return Channel.GetShipsYoungerThan(date);
        }
    }
}
