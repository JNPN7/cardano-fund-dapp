const express = require('express');
const exphbs = require('express-handlebars');
const routes = require('./routes/web');
const path = require('path');

const app = express();

const HTTP_PORT = 8000;
const baseUrl = `http://localhost:${HTTP_PORT}/`;

app.set("view engine", 'hbs');
const handlebars = exphbs.create({
	layoutDir: __dirname + '/views/layouts',
    extname: 'hbs',
    defaultLayout: 'main',
    
    helpers: {
        baseUrl: (path) => {
            return baseUrl + path
        },
    }
});

app.engine('hbs', handlebars.engine);

app.use(express.static('public'));
app.use(express.urlencoded({ extended: true }))

app.use(routes);

app.use((req, res) => {
	res.status(404).send('404');
});

app.listen(HTTP_PORT, () => {
	console.log(`Listening at port ${HTTP_PORT}`);
})

