#include <iostream>
#include <random>
#include <cmath>
#include <cassert>

const double PI = 3.141592653589793238463;

// (b) nbDraws = 100000, heu non 100 milion

/*
(c) const double circleRadius = 1.0;
    const double squareLenght = 10.0;
    const int nbDraws = 100000;

    les résultats sont moins précis
*/

void monteCarlo() {
    const double circleRadius = 1.0;
    const double squareLenght = 10.0;
    const int nbDraws = 100000;

    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<double> dis(-squareLenght / 2.0, squareLenght / 2.0);

    int inTheCircle = 0;
    for (int n = 0; n < nbDraws; ++n) {
        double x = dis(gen);
        double y = dis(gen);

        if (x*x + y*y <= circleRadius * circleRadius) {
            inTheCircle += 1;
        }
    }
    const double pi = std::pow(squareLenght, 2) * (double) inTheCircle / (double) nbDraws;

    std::cout << "pi = " << pi << std::endl;
}

void buffonNeedle() {
    const int nbDraws = 1000000;
    const double l = 2.0; // length of needles and space between strips
    const double floorLength = 10000.0;
    assert(floorLength > l);

    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<double> floorDis(0.0 + l, floorLength - l);
    std::uniform_real_distribution<double> angleDis(0.0, 10000);

    int nbIntersections = 0;

    for (int n = 0; n < nbDraws; ++n) {
        double x1 = floorDis(gen);
        double y1 = floorDis(gen);
        double angle = angleDis(gen);
        double x2 = x1 + std::cos(angle) * l;
        double y2 = y1 + std::sin(angle) * l;

        if (std::floor(x1 / l) != std::floor(x2 / l)) {
            nbIntersections += 1;
        }
    }
    
    const double p = (double) nbIntersections / (double) nbDraws;
    const double pi = 2.0 / p;
    std::cout << "pi = " << pi << std::endl;
}

void findE() {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<double> dis(0.0, 1.0);

    int NBDRAWS = 10000000;
    double sum = 0.0;

    for (int i = 0; i < NBDRAWS; ++i) {
        double total = 0.0;
        int nbDraws = 0;
        while (total < 1.0) {
            total += dis(gen);
            nbDraws += 1;
        }
        sum +=  nbDraws / total;
    }

    std::cout << "e = " << sum / (double) NBDRAWS << std::endl;
}

int main() {
    findE();
}
