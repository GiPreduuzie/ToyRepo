using System.ServiceModel;

namespace Contracts
{
    [ServiceContract]
    public interface IMessagingContract
    {
        [OperationContract]
        void GetMessage(string message);
    }
}
