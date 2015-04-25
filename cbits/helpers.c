#include <stdlib.h>
#include <stdio.h>
#include "../freetype-gl/texture-font.h"
#include "../freetype-gl/texture-atlas.h"

float *get_glyph_metrics(texture_glyph_t *glyph) {
    float *metrics = malloc(sizeof(float) * 9);

    metrics[0] = glyph->offset_x;
    metrics[1] = glyph->offset_y;
    metrics[2] = glyph->width;
    metrics[3] = glyph->height;
    metrics[4] = glyph->s0;
    metrics[5] = glyph->t0;
    metrics[6] = glyph->s1;
    metrics[7] = glyph->t1;
    metrics[8] = glyph->advance_x;

    return metrics;
}

unsigned int get_atlas_texture_id(texture_atlas_t *atlas) {
    return atlas->id;
}
