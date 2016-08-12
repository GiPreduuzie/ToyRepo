using System;

namespace RandomChoiceMonad.CollectionModifiers
{
    public static class CollectionModifiers
    {
        public static ICollectionModifier CreateRandomCollectionModifier(Random random)
        {
            return new RandomCollectionModifier(random);
        }

        public static ICollectionModifier CreateIdentityCollectionModifier()
        {
            return new IdenticalCollectionModifier();
        }
    }
}
