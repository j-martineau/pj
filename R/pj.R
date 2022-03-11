# prepare to build basic library of values
library(tidyverse)
library(readxl)

#' @name pj.
#' @title List of objects needed by the pjlot packages
#' @description \code{pj.} contains the following elements:
#'   \describe{
#'     \item{blank}{blank pj plot template object.}
#'     \item{patterns}{list of tibbles of patterns of argument characteristics.}
#'     \item{vals}{list of character vectors of valid argumemt values.}
#'   }
#' @export
pj. <- list(
  blank = list(
    element = tibble::tibble(class = "region", type = "bg", id = "region.0", name = "background", parent.id = "plot"),
    region = NULL,
    points = NULL,
    segments = NULL,
    reflines = NULL,
    area = NULL,
    ribbon = NULL,
    triangles = NULL,
    isosceles = NULL,
    rights = NULL,
    quads = NULL,
    rects = NULL,
    diamonds = NULL,
    kites = NULL,
    polys = NULL,
    regulars = NULL,
    stars = NULL,
    free = NULL,
    rounds = NULL,
    circles = NULL,
    ellipses = NULL,
    ci = NULL,
    ends = NULL,
    fills = NULL,
    glyphs = NULL,
    labels = NULL,
    lines = NULL,
    marks = NULL,
    dilate = NULL,
    reflect = NULL,
    rotate = NULL,
    translate = NULL,
    curr.ids  = list(region = 'region.0', shape = NULL, look = NULL, mod = NULL),
    next.ids  = list(region = 1, shape = 1, look = 1, mod = 1)
  ),
  patterns = list(
    region = readxl::read_excel('args.xlsx', sheet = 'region'),
    points = readxl::read_excel('args.xlsx', sheet = 'points'),
    segments = readxl::read_excel('args.xlsx', sheet = 'segments'),
    reflines = readxl::read_excel('args.xlsx', sheet = 'reflines'),
    area = readxl::read_excel('args.xlsx', sheet = 'area'),
    ribbon = readxl::read_excel('args.xlsx', sheet = 'ribbon'),
    triangles = readxl::read_excel('args.xlsx', sheet = 'triangles'),
    isosceles = readxl::read_excel('args.xlsx', sheet = 'isosceles'),
    rights = readxl::read_excel('args.xlsx', sheet = 'rights'),
    quads = readxl::read_excel('args.xlsx', sheet = 'quads'),
    rects = readxl::read_excel('args.xlsx', sheet = 'rects'),
    diamonds = readxl::read_excel('args.xlsx', sheet = 'diamonds'),
    kites = readxl::read_excel('args.xlsx', sheet = 'kites'),
    polys = readxl::read_excel('args.xlsx', sheet = 'polys'),
    regulars = readxl::read_excel('args.xlsx', sheet = 'regulars'),
    stars = readxl::read_excel('args.xlsx', sheet = 'stars'),
    free = readxl::read_excel('args.xlsx', sheet = 'free'),
    rounds = readxl::read_excel('args.xlsx', sheet = 'rounds'),
    circles = readxl::read_excel('args.xlsx', sheet = 'circles'),
    ellipses = readxl::read_excel('args.xlsx', sheet = 'ellipses'),
    ci = readxl::read_excel('args.xlsx', sheet = 'ci'),
    ends = readxl::read_excel('args.xlsx', sheet = 'ends'),
    fills = readxl::read_excel('args.xlsx', sheet = 'fills'),
    glyphs = readxl::read_excel('args.xlsx', sheet = 'glyphs'),
    labels = readxl::read_excel('args.xlsx', sheet = 'labels'),
    lines = readxl::read_excel('args.xlsx', sheet = 'lines'),
    marks = readxl::read_excel('args.xlsx', sheet = 'marks'),
    dilate = readxl::read_excel('args.xlsx', sheet = 'dilate'),
    reflect = readxl::read_excel('args.xlsx', sheet = 'reflect'),
    rotate = readxl::read_excel('args.xlsx', sheet = 'rotate'),
    translate = readxl::read_excel('args.xlsx', sheet = 'translate')
  ),
  vals = list(
    au = c('d', 'g', 'p', 'r'),
    bt = c('b','t'),
    ci = c('line', 'cap', 'taper', 'ribbon'),
    corner = c('bl', 'br', 'tl', 'tr'),
    edge = c('l', 'r', 'b', 't'),
    enc = c('c', 'd', 's', 'C', 'D', 'S'),
    end = c('s', 'm', 'e', 'b'),
    emark = c('c', 'd', 's', 't', '<'),
    font = c('mono', 'sans', 'serif', 'American Typewriter', 'Arial', 'Bradley Hand ITC', 'Calibri', 'Comic Sans MS', 'Courier', 'Courier New', 'Garamond', 'Geneva', 'Helvetica', 'Impact', 'Monaco', 'Palatino', 'Symbol', 'Tahoma', 'Times', 'Times New Roman', 'Trebuchet MS', 'Verdana'),
    hv = c('h', 'v'),
    ixy = c('i', 'x', 'y'),
    line = c('i', 'x', 'y', 'l', 'r', 'b', 't', 'h', 'v', 'd', 'u'),
    look = c('ci', 'ends', 'fills', 'glyphs', 'labels', 'lines', 'marks'),
    lr = c('l','r'),
    lt = c('-', ':', '=', ':=', 'L=', '2='),
    mark = c('c', 'd', 's', '<', '>', 'v', '^'),
    mod = c('dilate', 'reflect', 'rotate', 'translate'),
    point = c('c', 'o', 'bl', 'bm', 'br', 'lm', 'rm', 'tl', 'tm', 'tr'),
    pu = c('in', 'cm', 'mm'),
    seg = c('f', 'l', 'b', 'a'),
    shape = c('points', 'segments', 'reflines', 'area', 'ribbon', 'triangles', 'isosceles', 'rights', 'quads', 'rects', 'diamonds', 'kites', 'polys', 'regulars', 'stars', 'frees', 'rounds', 'circles', 'ellipses'),
    style = c('p', 'b', 'i', 'bi')
  )
)
