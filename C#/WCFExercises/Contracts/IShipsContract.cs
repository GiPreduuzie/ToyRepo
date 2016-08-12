using Core.Models;
using System;
using System.Collections.Generic;
using System.ServiceModel;

namespace Contracts
{
    [ServiceContract]
    public interface IShipsContract
    {
        [OperationContract(Name = "GetAllShips")]
        IEnumerable<Ship> GetShips();

        [OperationContract(Name = "GetShipsByName")]
        IEnumerable<Ship> GetShips(string name);

        [OperationContract]
        IEnumerable<Ship> GetShipsYoungerThan(DateTime date);
    }
}
