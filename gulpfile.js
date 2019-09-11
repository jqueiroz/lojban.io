'use strict';

const LESS_FILES = 'assets/less/**/*.less';
const JAVASCRIPT_FILES = 'assets/javascript/**/*.js';
const TYPESCRIPT_FILES = 'assets/typescript/**/*.ts';
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

var gulp = require('gulp');
var browserify = require('gulp-browserify');
var concat = require("gulp-concat");
var less = require('gulp-less');
var ts = require('gulp-typescript');
var uglify = require("gulp-uglify");

/* TASK'S */
gulp.task("fonts", function() {
    return gulp.src(FONT_FILES)
        .pipe(gulp.dest("./static/fonts"));
});

gulp.task("typescript", function () {
    return gulp.src(TYPESCRIPT_FILES)
        .pipe(ts())
        .pipe(browserify({
            insertGlobals : true
        }))
        .pipe(gulp.dest("./static/scripts"));
});

gulp.task("javascript", function () {
    return gulp.src(JAVASCRIPT_FILES)
        .pipe(gulp.dest("./static/scripts"));
});

gulp.task('scripts', gulp.series('typescript', 'javascript'));

gulp.task("styles", function(){
    return gulp.src(LESS_FILES)
        .pipe(less({
        }))
        .pipe(gulp.dest('./static/style'));
});

gulp.task("vendors:scripts", function () {
    return gulp.src(VENDORS_SCRIPTS_FILES)
        .pipe(concat("vendors.js"))
        .pipe(uglify())
        .pipe(gulp.dest("./static/scripts"));
});

gulp.task("vendors:styles", function() {
    return gulp.src(VENDORS_STYLES_FILES)
        .pipe(concat("vendors.css"))
        .pipe(gulp.dest("./static/style"));
});

gulp.task('default', gulp.series('fonts', 'scripts', 'styles', 'vendors:scripts', 'vendors:styles'));

gulp.task("watch", function () {
    gulp.watch(TYPESCRIPT_FILES, gulp.series("scripts"));
    gulp.watch(LESS_FILES, gulp.series("styles"));
});
