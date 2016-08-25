using System.Collections.Generic;
using System.Linq;

namespace TreeBuilder
{
    public static class FormatHelper
    {
        public static string AddIdent(string text)
        {
            var ident = "     ";
            return ident + text.Replace("\r\n", "\r\n" + ident);
        }

        public static string JoinAndIdent(IEnumerable<object> items)
        {
            return AddIdent(string.Join("\r\n", items));
        }

        public static string Join(params string[] items)
        {
            return string.Join("\r\n", items.Where(x => !string.IsNullOrWhiteSpace(x)));
        }
    }
}