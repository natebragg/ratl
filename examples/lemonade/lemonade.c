#include <stdio.h>
#include <coin/Clp_C_Interface.h>

int main()
{
    Clp_Simplex *model = Clp_newModel();
    Clp_setLogLevel(model, 0);

    int status = Clp_readMps(model, "lemonade.mps", 1, 0);
    if (status) {
        return status;
    }

    Clp_setOptimizationDirection(model, -1);

    status = Clp_initialSolve(model);
    if (status) {
        return status;
    }

    printf("Solution: opt %d, ppi %d, pdi %d, plr %d, dlr %d, ilr %d, abn %d\n",
        Clp_isProvenOptimal(model),
        Clp_isProvenPrimalInfeasible(model),
        Clp_isProvenDualInfeasible(model),
        Clp_isPrimalObjectiveLimitReached(model),
        Clp_isDualObjectiveLimitReached(model),
        Clp_isIterationLimitReached(model),
        Clp_isAbandoned(model));

    const double * pr = Clp_getRowActivity(model);
    int nr = Clp_getNumRows(model);
    for (int row = 0; row < nr; ++row) {
        printf("row %d, value %f\n", row, pr[row]);
    }

    const double * pc = Clp_getColSolution(model);
    int nc = Clp_getNumCols(model);
    for (int col = 0; col < nc; ++col) {
        printf("col %d, solution %f\n", col, pc[col]);
    }

    Clp_deleteModel(model);
    return 0;
}
