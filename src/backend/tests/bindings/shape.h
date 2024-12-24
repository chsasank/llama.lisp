typedef struct {
    int x;
    int y;
} point_t;

typedef struct rect {
    point_t top_left;
    point_t bottom_right;
} rect_t;

/*int area_of (rect_t * rect) {
    int area = 1;
    area *= (rect->bottom_right->x - rect->top_left->x);
    area *= (rect->bottom_right->y - rect->top_left->y);
    return area;
} */
