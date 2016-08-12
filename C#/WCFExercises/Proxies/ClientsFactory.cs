using Contracts;

namespace Proxies
{
    public class ClientFactory
    {
        public IShipsContract GetShipsClient()
        {
            return new ShipsContractClient();
        }
    }
}
