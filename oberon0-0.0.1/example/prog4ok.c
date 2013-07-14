typedef int _hola_matrix[10][10];
typedef struct {
            int f1; int f2[3];
        } _hola_rec;
int _hola_0case;
int _hola_a1[5];
_hola_rec _hola_r1;
_hola_matrix _hola_m1;
int _hola_i, _hola_j;
void _hola_start(_hola_matrix (*_hola_1start_m))
{
    int _hola_1start_0case;
    int _hola_1start_i, _hola_1start_j;
    _hola_1start_i = 0;
    while (_hola_1start_i <= 9)
    {
        _hola_1start_j = 0;
        while (_hola_1start_j <= 9)
        {
            (*_hola_1start_m)[_hola_1start_i][_hola_1start_j] = _hola_1start_i * _hola_1start_j;
            ;
            _hola_1start_j = _hola_1start_j + 1;
        }
        ;
        _hola_1start_i = _hola_1start_i + 1;
    }
    ;
}
int main()
{
    _hola_a1[0] = 10;
    _hola_r1.f1 = 9;
    _hola_r1.f2[0] = 8;
    _hola_start(&_hola_m1);
    _hola_i = 0;
    while (_hola_i <= 9)
    {
        _hola_j = 0;
        while (_hola_j <= 9)
        {
            printf("%d", _hola_m1[_hola_i][_hola_j]);
            ;
            _hola_j = _hola_j + 1;
        }
        printf("\n");
        ;
        _hola_i = _hola_i + 1;
    }
    ;
    return 0;
}