namespace RandomChoiceMonad.Model
{
    public class ModelRepository
    {
        public Matrix[] GetTestModel()
        {
            return new Matrix[]
            {
                new Matrix {
                    Name = "1",
                    Surveys = new [] {
                        new Survey { Name = "1", Questions = new Question[0] },
                        new Survey { Name = "2",
                            Questions = new [] {
                                new Question { Name = "1*", Fields = new Field[0]} } },
                        new Survey { Name = "3",
                            Questions = new [] {
                                new Question { Name = "1", Fields = new[] { new Field {Name = "1" }, new Field { Name = "1" } }},
                                new Question { Name = "2", Fields = new Field[0]} } },
                        new Survey { Name = "4", Questions = new Question[0] }
                } },
            };
        }
    }
}
