const express = require('express');
const path = require('path');
const httpRouting = require('./controllers/statusController');

const app = express();
const port = process.env.PORT || 3000;

// Disable false status codes due to cache
app.set('etag', false);

// Serve static files from the public folder
app.use(express.static(path.join(__dirname, '../public')));

// Landing page route
app.get('/', (req, res) => {
    res.sendFile(path.join(__dirname, '../public/index.html'));
});

// Ping-Pong 
app.get('/ping', (req, res) => {
    res.send("pong")
});
// HTTP status code route
app.use('/http', httpRouting);

// Start the server
app.listen(port, () => {
    console.log(`Server is running on http://localhost:${port}`);
});
