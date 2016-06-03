var gulp = require('gulp')
var util = require('gulp-util')
var async = require('async')
var mkdirp = require('mkdirp')
var rimraf = require('rimraf')
var gulpStylus = require('gulp-stylus')
var gulpJade = require('gulp-jade')
var gulpElm  = require('gulp-elm')
var sourcemaps = require('gulp-sourcemaps')
var connect = require('connect')
var tinylr = require('tiny-lr')
var cors = require('cors')
var prefix = require('gulp-autoprefixer')
var pushState = require('connect-pushstate')
var injectLr = require('connect-livereload')
var serveStatic = require('serve-static')
var http = require('http')

var HTTP_PORT = 8080
var LR_PORT = 35729

gulp.task('elm-init', elm.init)

gulp.task('default', [ 'elm-init' ], function(done) {
    async.series([ clean, build, serve, liveReload, watch ], function(err) {
        if (err) {
            console.error(err && (err.stack || err))
        }
        else {
            gutil.log('Ready')
        }
    })
})

function clean (done) {
    util.log('clean')
    rimraf('dist', done)
}

function build (done) {
    util.log('build')
    async.parallel([ elm, stylus, jade ], done)
}

function elm (done) {
    util.log('elm')
    gulp.src('src/elm/*.elm')
        .pipe(gulpElm.bundle('bundle.js'))
        .on('error', handleError)
        .pipe(gulp.dest('dist'))
        .on('end', done)
}

function stylus (done) {
    util.log('stylus')
    gulp.src('src/stylus/index.styl')
        .pipe(sourcemaps.init())
        .pipe(gulpStylus({
            'sourcemap': true,
            'include css': true,
        }))
        .on('error', handleError)
        .pipe(prefix([ 'last 2 versions' ], { cascade: true }))
        .pipe(gulp.dest('dist'))
        .on('end', done)
}

function jade (done) {
    util.log('jade')
    gulp.src('src/index.jade')
        .pipe(gulpJade({
            pretty: false,
            locals: {},
        }))
        .on('error', handleError)
        .pipe(gulp.dest('dist'))
        .on('end', function() {
            done()
        })
}

function serve (done) {
    util.log('serve')
    const app = connect()
        .use(pushState())
        .use(injectLr())
        .use(cors())
        .use(serveStatic('dist'))

    staticServer = http.createServer(app)
    staticServer.listen(HTTP_PORT, done)
}

function liveReload () {
    util.log('livereload')
    tinylr().listen(LR_PORT)
}

function triggerLivereload (type) {
    util.log('Reloading: ' + type)
    var query = ''
    if (type === 'all') query = 'files=index.html'
    if (type === 'css') query = 'files=index.css'
    var url = 'http://127.0.0.1:' + LR_PORT
    http.get(url + '/changed?' + query)
}

var triggerReload = triggerLivereload.bind(null, 'all')
var triggerCssReload = triggerLivereload.bind(null, 'css')

function watch () {
    util.log('watch')
    watchElm()
    watchStylus()
    watchJade()
}

function watchElm () {
    util.log('watch elm')
    gulp.watch('src/elm/**/*', elm.bind(null, triggerReload))
}

function watchStylus () {
    util.log('watch stylus')
    gulp.watch('src/stylus/**/*', stylus.bind(null, triggerCssReload))
}

function watchJade () {
    util.log('watch jade')
    gulp.watch('src/index.jade', jade.bind(null, triggerReload))
}

function handleError(err) {
    console.error(err && (err.stack || err))
}
