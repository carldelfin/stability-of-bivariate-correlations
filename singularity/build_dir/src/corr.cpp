#include <fstream>
#include <json.hpp>
#include <armadillo>
#include <cmath>

// NOTE!
// compile with:
// g++ -I path/containing/json.hpp corr.cpp -o cppcorr -larmadillo -llapack -lblas

using json = nlohmann::json;
using namespace std;

int main(int argc, char **argv)
{
    // read JSON data
    string fn = argv[1];
    std::ifstream f(fn);
    json data = json::parse(f);

    // create arma vectors for x y and N
    auto tmpx = data["x"].get<std::vector<double>>();
    arma::vec x = arma::conv_to<arma::vec>::from(tmpx);
    
    auto tmpy = data["y"].get<std::vector<double>>();
    arma::vec y = arma::conv_to<arma::vec>::from(tmpy);
    
    const int N = data["N"];

    // estimate rho
    double r = arma::as_scalar(arma::cor(x, y));
    
    // calculate confidence intervals
    double z = 0.5 * log((1.0 + r) / (1.0 - r));
    double pr = sqrt(1.0 / (N - 3.0));

    double lower95 = z - (pr * 1.959964);
    double upper95 = z + (pr * 1.959964);
    double confl95 = (exp(2.0 * lower95) - 1.0) / (exp(2.0 * lower95) + 1.0);
    double confu95 = (exp(2.0 * upper95) - 1.0) / (exp(2.0 * upper95) + 1.0);
    
    double lower90 = z - (pr * 1.644854);
    double upper90 = z + (pr * 1.644854);
    double confl90 = (exp(2.0 * lower90) - 1.0) / (exp(2.0 * lower90) + 1.0);
    double confu90 = (exp(2.0 * upper90) - 1.0) / (exp(2.0 * upper90) + 1.0);
    
    double lower66 = z - (pr * 0.9541653);
    double upper66 = z + (pr * 0.9541653);
    double confl66 = (exp(2.0 * lower66) - 1.0) / (exp(2.0 * lower66) + 1.0);
    double confu66 = (exp(2.0 * upper66) - 1.0) / (exp(2.0 * upper66) + 1.0);

    // output
    std::cout << r << " ";
    std::cout << confl95 << " ";
    std::cout << confu95 << " ";
    std::cout << confl90 << " ";
    std::cout << confu90 << " ";
    std::cout << confl66 << " ";
    std::cout << confu66 << std::endl;
}
