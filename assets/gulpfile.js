'use strict';

// TODO: migrate to gulp-babel instead of using gulp-react

const LESS_FILES = './less/**/*.less';
const JAVASCRIPT_FILES = './javascript/**/*.js';
const TYPESCRIPT_FILES = './typescript/**/*.ts';//, './typescript/**/*.tsx'];
const FONT_FILES = [
    './node_modules/bootstrap/fonts/*.*',
    './node_modules/font-awesome/fonts/*.*'
];
const VENDORS_SCRIPTS_FILES = [
    './node_modules/jquery/dist/jquery.js',
    './node_modules/bootstrap/dist/js/bootstrap.min.js'
];
const VENDORS_STYLES_FILES = [
    './node_modules/bootstrap/dist/css/bootstrap.min.css',
    './node_modules/font-awesome/css/font-awesome.min.css'
];

var browserify = require('browserify');
var buffer = require('vinyl-buffer');
var concat = require("gulp-concat");
var glob = require('glob');
var gulp = require('gulp');
var less = require('gulp-less');
var react = require('gulp-react');
var source = require('vinyl-source-stream');
var sourcemaps  = require('gulp-sourcemaps');
var uglify = require("gulp-uglify");

/* TASK'S */
gulp.task("fonts", function () {
    return gulp.src(FONT_FILES)
        .pipe(gulp.dest("../static/fonts"));
});

gulp.task("typescript", function () {
    return glob(TYPESCRIPT_FILES, function(err, files){
        if(err) return;
        files.map(function(entry) {
            browserify({ entries: [entry] })
                .transform("babelify", { presets: ["es2015"] })
                .plugin("tsify")
                .bundle()
                .pipe(source(entry.replace('/typescript', '').replace('ts', 'js')))
                .pipe(buffer())
                .pipe(sourcemaps.init({ loadMaps: true }))
                .pipe(sourcemaps.write('./'))
                .pipe(gulp.dest('../static/scripts'));
        })
    });
});

gulp.task("javascript", function () {
    return gulp.src(JAVASCRIPT_FILES)
        .pipe(gulp.dest("../static/scripts"));
});

gulp.task('scripts', gulp.series('typescript', 'javascript'));

gulp.task("styles", function () {
    return gulp.src(LESS_FILES)
        .pipe(less({
        }))
        .pipe(gulp.dest('../static/style'));
});

gulp.task("vendors:scripts", function () {
    return gulp.src(VENDORS_SCRIPTS_FILES)
        .pipe(concat("vendors.js"))
        .pipe(uglify())
        .pipe(gulp.dest("../static/scripts"));
});

gulp.task("vendors:styles", function () {
    return gulp.src(VENDORS_STYLES_FILES)
        .pipe(concat("vendors.css"))
        .pipe(gulp.dest("../static/style"));
});

gulp.task('default', gulp.series('fonts', 'scripts', 'styles', 'vendors:scripts', 'vendors:styles'));

gulp.task("watch", function () {
    gulp.watch(TYPESCRIPT_FILES, gulp.series("scripts"));
    gulp.watch(LESS_FILES, gulp.series("styles"));
});
