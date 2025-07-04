#include <stdio.h>
#include <math.h>

// Global variables for matrix dimensions
int m, n;

// To print matrix
void printmat(float arr[m][n]) {
    printf("\n\n");
    for (int i = 0; i < m; i++) {
        for (int j = 0; j < n; j++) {
            if (fabs(arr[i][j]) < 0.001) {
                arr[i][j]=0;
            }
            printf("  %.2f  ", arr[i][j]);
        }
        printf("\n");
    }
    printf("\n\n");
}

// To print inverse matrix
void printinvmat(float arr[m][2 * n]) {
    printf("\n\n");
    for (int i = 0; i < m; i++) {
        for (int j = 0; j < n; j++) {
             if (fabs(arr[i][j]) < 0.001) {
                arr[i][j]=0;
            }
            printf("  %.2f  ", arr[i][j]);
        }
        printf("\n");
    }
    printf("\n\n");
}

// Taking input from user
void takeinput(float arr[m][n]) {
    printf("Enter elements in form of matrix: \n");
    for (int i = 0; i < m; i++) {
        for (int j = 0; j < n; j++) {
            scanf("%f", &arr[i][j]);
        }
    }
}

void rank(float arr[m][n]) {
reducedRowEchelonForm(arr);
int rank = 0;
for (int i = 0; i < m; i++) {
    for (int j = 0; j < n; j++) {
        if(fabs(arr[i][j]) > 0.001) {
            rank++;
            break;
        }
    }
}

printf("Rank is: %d \n",rank);
}

// To perform row operations: Modify row 'a' by adding or subtracting a multiple of row 'b', starting from the 'lead' column
void ropt(float arr[m][n], int a, int b, int lead,char choice) {
    float first = arr[a][lead];
    char o = (first > 0) ? '-' : '+';
    for (int i = lead; i < n; i++) {
        arr[a][i] -= arr[b][i] * first;
    }
    if(choice=='r'||choice=='R'||choice=='e'||choice=='E'){
    printf("Row %d %c %.2f X Row %d\n", a + 1, o, fabs(first), b + 1);
    printmat(arr);
    }
}

// To swap rows a and b in a matrix
void swaprow(float arr[m][n], int a, int b,char choice) {
    for (int i = 0; i < n; i++) {
        float temp = arr[a][i];
        arr[a][i] = arr[b][i];
        arr[b][i] = temp;
    }
    if(choice=='r'||choice=='R'||choice=='e'||choice=='E'){
    printf("Row swap between Row %d and Row %d\n", a + 1, b + 1);
    printmat(arr);
    }
}

// To convert first element to 1 in a row and lead is the column containing the first element
void convertto1(float arr[m][n], int a, int lead,char choice) {
    float div = arr[a][lead];
    for (int i = lead; i < n; i++) {
        if (fabs(arr[a][i])>0.001) {
            arr[a][i] /= div;
        }
    }
    if(choice=='r'||choice=='R'||choice=='e'||choice=='E'){
    printf("Division of row %d by %.2f\n", a + 1, div);
    printmat(arr);
    }
}

// Row Echelon Form (REF) calculation
void rowEchelonForm(float arr[m][n], char choice) {
    int lead = 0;
    for (int i = 0; i < m && lead < n; i++) {
        if (arr[i][lead] == 0) {
            int swap = 0;
            for (int t = i + 1; t < m; t++) {
                if (arr[t][lead] != 0) {
                    swaprow(arr, i, t,choice);
                    swap = 1;
                    break;
                }
            }
            if (swap==0) {
                lead++;
                i--;
                continue;
            }
        }
        if (arr[i][lead] != 1) {
            convertto1(arr, i, lead,choice);
        }
        for (int k = i + 1; k < m; k++) {
            if (arr[k][lead] != 0) {
                ropt(arr, k, i, lead,choice);
            }
        }
        lead++;
    }
}

// Reduced Row Echelon Form (RREF) calculation
void reducedRowEchelonForm(float arr[m][n],char choice) {
    rowEchelonForm(arr,choice);
    for (int i = m - 1; i >= 0; i--) {
        for (int j = 0; j < n; j++) {
            if (arr[i][j] == 1) {
                for (int k = 0; k < i; k++) {
                    if (arr[k][j] != 0) {
                        ropt(arr, k, i, j,choice);
                    }
                }
                break;
            }
        }
    }
}

// Function to calculate the inverse of a matrix
void inverseMatrix(float arr[m][n],char choice) {
    if (m != n) {
        printf("Inverse can only be calculated for square matrices.\n");
        return;
    }

    float arr2[m][n];
    for (int i = 0; i < m; i++){
        for (int j = 0; j < n; j++){
                arr2[i][j]=arr[i][j];
        }
    }

    // Check if RREF is identity matrix
    reducedRowEchelonForm(arr,choice);
    for (int i = 0; i < m; i++){
        for (int j = 0; j < n; j++){
                         if (fabs(arr[i][j]) < 0.001) {
                arr[i][j]=0;
            }
        }
    }
     int k = 0;
    for (int i = 0; i < n; i++){
            if(arr[m-1][i] == 0){
                k++;
            }
        }

    if(k == n) {
        printf("Matrix is not invertible (singular matrix)\n");
        return;
    }

    else{

    // Create an augmented matrix
    float augmented[m][2 * n];

    // Fill the augmented matrix with the original matrix and the identity matrix
    for (int i = 0; i < m; i++) {
        for (int j = 0; j < n; j++) {
            augmented[i][j] = arr2[i][j];
        }
        for (int j = n; j < 2 * n; j++) {
            augmented[i][j] = (j - n == i) ? 1 : 0; // Identity matrix
        }
    }
    printf("Augmented Matrix before Reduction: \n");
        for (int i = 0; i < m; i++) {
        for (int j = 0; j < 2 * n; j++) {
            printf("  %.2f  ", augmented[i][j]);
        }
        printf("\n");
    }
    printf("\n\n");

    // Perform RREF on the augmented matrix
    invreducedRowEchelonForm(augmented,choice);
    printf("RREF of Augmented Matrix: \n");
    for (int i = 0; i < m; i++) {
        for (int j = 0; j < 2 * n; j++) {
             if (fabs(augmented[i][j]) < 0.001) {
                augmented[i][j] = 0;
            }
            printf("  %.2f  ", augmented[i][j]);
        }
        printf("\n");
    }
    printf("\n\n");

    // Print the inverse matrix
    printf("The inverse of the matrix is:\n");
    for (int i = 0; i < m; i++) {
        for (int j = n; j < 2 * n; j++) {
            printf("  %.2f  ", augmented[i][j]);
            }
        printf("\n");
        }

    }
}

void invropt(float arr[m][2 * n], int a, int b, int lead,char choice) {
    float first = arr[a][lead];
    for (int i = lead; i < 2 * n; i++) {
        arr[a][i] -= arr[b][i] * first;
    }
}

void invswaprow(float arr[m][2 * n], int a, int b,char choice) {
    for (int i = 0; i < 2 * n; i++) {
        float temp = arr[a][i];
        arr[a][i] = arr[b][i];
        arr[b][i] = temp;
    }
}

void invconvertto1(float arr[m][2 * n], int a, int lead,char choice) {
    float div = arr[a][lead];
    for (int i = lead; i < 2 * n; i++) {
        if (fabs(arr[a][i]) > 0.001) {
            arr[a][i] /= div;
        }
    }
}

void invrowEchelonForm(float arr[m][2 * n], char choice) {
    int lead = 0;
    for (int i = 0; i < m && lead < 2 * n; i++) {
        if (arr[i][lead] == 0) {
            int swap = 0;
            for (int t = i + 1; t < m; t++) {
                if (arr[t][lead] != 0) {
                    invswaprow(arr, i, t,choice);
                    swap = 1;
                    break;
                }
            }
            if (swap == 0) {
                lead++;
                i--;
                continue;
            }
        }
        if (arr[i][lead] != 1) {
            invconvertto1(arr, i, lead,choice);
        }
        for (int k = i + 1; k < m; k++) {
            if (arr[k][lead] != 0) {
                invropt(arr, k, i, lead,choice);
            }
        }
        lead++;
    }
}

void invreducedRowEchelonForm(float arr[m][2 * n],char choice) {
    invrowEchelonForm(arr,choice);
    for (int i = m - 1; i >= 0; i--) {
        for (int j = 0; j < 2 * n; j++) {
            if (arr[i][j] == 1) {
                for (int k = 0; k < i; k++) {
                    if (arr[k][j] != 0) {
                        invropt(arr, k, i, j,choice);
                    }
                }
                break;
            }
        }
    }
}

int main() {
    // Input dimensions
    printf("Enter the number of rows: ");
    scanf("%d", &m);
    printf("Enter the number of columns: ");
    scanf("%d", &n);
    float arr[m][n];

    // Taking input
    takeinput(arr);
    printf("Original matrix is:");
    printmat(arr);

    // Ask user whether to perform REF, RREF, or inverse calculation
    char choice;
    restart:
    printf("Press:\nR- To find Row Echelon Form (REF) of the matrix\nE- To find Reduced Row Echelon Form (RREF) of the matrix\nI- To find Inverse of the matrix\n");
    scanf(" %c", &choice);

    if (choice == 'R' || choice == 'r') {
        rowEchelonForm(arr,choice);
        printf("Row Echelon Form of the matrix is: ");
        printmat(arr);
        rank(arr);
    } else if (choice == 'E' || choice == 'e') {
        reducedRowEchelonForm(arr,choice);
        printf("Reduced Row Echelon Form of the matrix is: ");
        printmat(arr);
        rank(arr);
    } else if (choice == 'I' || choice == 'i') {
        inverseMatrix(arr,choice);
    } else {
        printf("Invalid choice! Please restart and choose either R, E, or I.\n");
        goto restart;
    }

    return 0;
}
