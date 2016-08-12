using Proxies;
using System;
using System.Linq;
using System.Windows;

namespace ShipsClient
{
    /// <summary>
    /// Логика взаимодействия для MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
        }

        private void LoadShipsButton_Click(object sender, RoutedEventArgs e)
        {
            var shipsClient = new ClientFactory().GetShipsClient();
            var ships = shipsClient.GetShips();

            OutputLabel.Content = string.Join(Environment.NewLine, ships.Select(x => x.Name + "\t" + x.Launched));
        }
    }
}
