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

// (b) N = 10000000
// (c) Elles semblent équivalentes.

void findE() {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<double> dis(0.0, 1.0);

    const int N = 10000000;

    double sum_n = 0.0;
    for (int i = 0; i < N; ++i) {
        double sum_r = 0.0;
        int n = 0;

        while (sum_r < 1.0) {
            double r = dis(gen);
            sum_r += r;
            n += 1;
        }

        sum_n +=  n;
    }

    double sum_m = 0.0;
    for (int i = 0; i < N; ++i) {
        int m = 1;

        double r = dis(gen);
        while (true) {
            double r2 = dis(gen);

            m += 1;

            if (r2 < r) {
                break;
            }

            r = r2;
        }

        sum_m +=  m;
    }

    std::cout << "e = " << sum_n / (double) N << " ou " << sum_m / (double) N << std::endl;
}

struct Vertex {
    double x = 0, y = 0;
};

struct Vector {
    double x = 0, y = 0;
};

using Polygon = std::vector<Vertex>;

struct Segment {
    Vertex v1, v2;
};

Polygon getPolygonFromInput() {
    size_t nbVertices;
    std::cout << "Nombre de sommets: ";
    std::cin >> nbVertices;

    std::vector<Vertex> vertices(nbVertices);
    for (size_t i = 0; i < nbVertices; ++i) {
        Vertex v;

        std::cout << "x: ";
        std::cin >> v.x;
        std::cout << "y: ";
        std::cin >> v.y;

        vertices.push_back(v);
    }

    return vertices;
}

Vertex getFarVertex(const Polygon& p) {
    Vertex far;

    for (const Vertex& v : p) {
        far.x = std::max(far.x, v.x);
        far.y = std::max(far.y, v.y);
    }

    far.x *= 2;
    far.y *= 2;

    return far;
}

double produit_vectoriel(const Vector& a, const Vector& b) {
    return a.x * b.y - a.y * b.x;
}

Vector vecteur_segment(const Vertex& A, const Vertex& B) {
    return Vector{B.x - A.x, B.y - A.y};
}

template <typename T> int sign(T val) {
    return (T(0) < val) - (val < T(0));
}

bool collide(const Segment& s1, const Segment& s2) {
    Vertex A(s1.v1), B(s1.v2), C(s2.v1), D(s2.v2);

    Vector AB = vecteur_segment(A, B),
    AC = vecteur_segment(A, C),
    AD = vecteur_segment(A, D),

    CD = vecteur_segment(C, D),
    CA = vecteur_segment(C, A),
    CB = vecteur_segment(C, B);

    double pABAC = produit_vectoriel(AB, AC),
    pABAD = produit_vectoriel(AB, AD),
    pCDCA = produit_vectoriel(CD, CA),
    pCDCB = produit_vectoriel(CD, CB);

    return (sign(pABAC) != sign(pABAD)) and (sign(pCDCA) != sign(pCDCB));
}

bool in(const Vertex& v, const Polygon& p) {
    Segment s1{v, getFarVertex(p)};
    int nbCollide = 0;

    for (size_t i = 0; i < p.size(); ++i) {
        Segment s2{p[i], p[(i+1)%p.size()]};

        if (collide(s1, s2)) {
            nbCollide += 1;
        }
    }

    return nbCollide % 2 != 0;
}

Vertex min(const Polygon& p) {
    Vertex r(p[0]);
    for (size_t i = 1; i < p.size(); ++i) {
        r.x = std::min(r.x, p[i].x);
        r.y = std::min(r.y, p[i].y);
    }
    return r;
}

Vertex max(const Polygon& p) {
    Vertex r(p[0]);
    for (size_t i = 1; i < p.size(); ++i) {
        r.x = std::max(r.x, p[i].x);
        r.y = std::max(r.y, p[i].y);
    }
    return r;
}

double monteCarloPolygon(const Polygon& p) {
    const int nbDraws = 100000;

    Vertex vMin(min(p)), vMax(max(p));

    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<double> disX(vMin.x, vMax.x), disY(vMin.y, vMax.y);

    int inTheRectangle = 0;
    for (int n = 0; n < nbDraws; ++n) {
        if (in({disX(gen), disY(gen)}, p)) {
            inTheRectangle += 1;
        }
    }

    return (vMax.x - vMin.x) * (vMax.y - vMin.y) * (double) inTheRectangle / (double) nbDraws;
}

double shoelace(const Polygon& p) {
    double sum1 = 0, sum2 = 0;
    for (size_t i = 0; i < p.size(); ++i) {
        sum1 += p[i].x * p[(i+1)%p.size()].y;
        sum2 += p[i].y * p[(i+1)%p.size()].x;
    }
    return std::abs(0.5 * (sum1 - sum2));
}

int main() {
    Polygon p{{1, 3}, {3, 5}, {6, 4}, {5, 2}, {2, 1}}, p2{{5, 1}, {5, 4}, {1, 4}, {1, 1}};

    std::cout << "area: " << monteCarloPolygon(p) << " ou " << shoelace(p) << std::endl;
    std::cout << "area: " << monteCarloPolygon(p2) << " ou " << shoelace(p2) << std::endl;
}
