#include <fstream>
#include <vector>
#include <memory>
#include <sstream>
#include <cmath>
#include <cassert>
#include <iostream>
#include <glpk.h>             /* GNU GLPK linear/mixed integer solver */

struct Problem {
    struct Initial {
        uint width;
        double something;
    };

    struct Final {
        uint count;
        uint width;
    };

    const std::vector<Initial> initials;
    const std::vector<Final> finals;
};

std::unique_ptr<Problem> read_problem(std::istream& s) {
    std::vector<Problem::Initial> initials;
    std::vector<Problem::Final> finals;
    std::string line;

    std::getline(s, line);
    if (line != "initials")
        return nullptr;

    while (std::getline(s, line)) {
        std::istringstream iss(line);
        Problem::Initial i;
        if (!(iss >> i.width >> i.something)) { break; }
        initials.push_back(i);
    }

    if (line != "finals")
        return nullptr;

    while (std::getline(s, line)) {
        std::istringstream iss(line);
        Problem::Final f;
        if (!(iss >> f.width >> f.count)) { return nullptr; }
        finals.push_back(f);
    }

    return std::make_unique<Problem>(Problem {initials, finals});
}

class Patterns {
public:
    Patterns(size_t nbLines) : nbLines(nbLines) {}

    void setNbPatterns(size_t nbPatterns) {
        data.resize(nbLines * nbPatterns, 0.f);
    }

    double& get(size_t line, size_t column) {
        assert(column * nbLines + line < data.size());
        return data[column * nbLines + line];
    }

    const double& get(size_t line, size_t column) const {
        assert(column * nbLines + line < data.size());
        return data[column * nbLines + line];
    }

    size_t getNbLines() const {
        return nbLines;
    }

    size_t getNbPatterns() const {
        return data.size() / nbLines;
    }

    void print() const {
        for (size_t i = 0; i < getNbLines(); ++i) {
            for (size_t j = 0; j < getNbPatterns(); ++j) {
                std::cout << get(i, j) << " ";
            }
            std::cout << std::endl;
        }
    }

    void pushPattern(double* lines) {
        setNbPatterns(getNbPatterns() + 1);
        size_t column = getNbPatterns() - 1;
        for (size_t line = 0; line < nbLines; ++line) {
            get(line, column) = lines[line];
        }
    }

    void loadMatrix(glp_prob *lp) const {
        size_t nm = getNbLines() * getNbPatterns();
        std::vector<int> ia(nm), ja(nm);

        size_t k = 0;
        for (size_t j = 0; j < getNbPatterns(); ++j) {
            for (size_t i = 0; i < getNbLines(); ++i) {
                ia[k] = i + 1;
                ja[k] = j + 1;
                ++k;
            }
        }

        glp_load_matrix(lp, nm, ia.data()-1, ja.data()-1, data.data()-1);
    }

private:
    const size_t nbLines;
    std::vector<double> data;
};

struct Result {
    double objective;
    std::vector<double> primals;
    std::vector<double> duals;

    void print() {
        std::cout << "objective: " << objective << std::endl;
        std::cout << "primals: ";
        for (double p : primals) {
            std::cout << p << " ";
        }
        std::cout << std::endl;
        std::cout << "duals: ";
        for (double d : duals) {
            std::cout << d << " ";
        }
        std::cout << std::endl;
    }
};

Result solve(const std::vector<Problem::Final>& finals, const Patterns& patterns) {
    /* declare variables */
    glp_prob *lp;
    /* create problem */
    lp = glp_create_prob();
    glp_set_obj_dir(lp, GLP_MIN);
    /* fill problem */
    int rows = static_cast<int>(finals.size());
    glp_add_rows(lp, rows);
    for (int row = 0; row < rows; ++row) {
        glp_set_row_bnds(lp, row + 1, GLP_LO, finals[row].count, 0.0);
    }

    int columns = patterns.getNbPatterns();
    glp_add_cols(lp, columns);
    for (int column = 1; column <= columns; ++column) {
        glp_set_col_bnds(lp, column, GLP_LO, 0.0, 0.0);
        glp_set_obj_coef(lp, column, 1.0);
    }

    patterns.loadMatrix(lp);

    /* solve problem */
    glp_simplex(lp, nullptr);

    /* recover and display results */
    Result result;
    result.objective = glp_get_obj_val(lp);

    result.primals.resize(columns);
    for (int column = 1; column <= columns; ++column) {
        result.primals[column - 1] = glp_get_col_prim(lp, column);
    }

    result.duals.resize(rows);
    for (int row = 1; row <= rows; ++row) {
        result.duals[row - 1] = glp_get_row_dual(lp, row);
    }

    /* housekeeping */
    glp_delete_prob(lp);
    glp_free_env();

    return result;
}

int main()
{
  std::ifstream file("instance1");
  auto problem = read_problem(file);

  if (!problem)
      return -1;

  auto initial = problem->initials.front();
  auto finals = problem->finals;

  Patterns patterns(finals.size());
  patterns.setNbPatterns(finals.size());

  for (size_t i = 0; i < finals.size(); ++i) {
      patterns.get(i, i) = initial.width / finals[i].width;
  }

  patterns.print();

  solve(finals, patterns).print();

  return 0;
}

