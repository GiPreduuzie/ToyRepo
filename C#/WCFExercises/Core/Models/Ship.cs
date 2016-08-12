using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Core.Models
{
    public class Ship
    {
        public ShipTypes ShipType { get; set; }

        public string Name { get; set; }

        public DateTime LaunchedDate { get; set; }

        public DateTime LastReconstructionDate { get; set; }
    }
}
