using Core.Models;
using System;
using System.Collections.Generic;

namespace Core.Repositories
{
    public interface IShipRepository
    {
        IEnumerable<Ship> GetShips();

        IEnumerable<Ship> GetShips(string name);

        IEnumerable<Ship> GetShipsYoungerThan(DateTime date);
    }
 }