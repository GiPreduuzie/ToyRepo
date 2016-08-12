using System;
using System.Collections.Generic;
using System.Linq;

namespace RandomChoiceMonad.CollectionModifiers
{
    internal class RandomCollectionModifier : ICollectionModifier
    {
        Random _random;

        public RandomCollectionModifier(Random random)
        {
            _random = random;
        }

        public IEnumerable<T> Modify<T>(IEnumerable<T> collection)
        {
            return Randomize<T>(collection);
        }

        private IEnumerable<TItem> Randomize<TItem>(IEnumerable<TItem> set)
        {
            var list = set.ToList();

            while (list.Any())
            {
                var next = _random.Next(list.Count);
                var result = list[next];
                list.RemoveAt(next);
                yield return result;
            }
        }
    }
}
