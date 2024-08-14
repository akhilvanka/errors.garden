const htmlTemplate = require('../views/template');
const httpRouting = require('express').Router();

// Helper function to check if user_agent is a web_browser
const isBrowser = (req) => {
    const userAgent = req.headers['user-agent'];
    return /Mozilla|Chrome|Safari|Edge|Opera/.test(userAgent);
};


// MARK: GET Requests 
httpRouting.get('/', (req, res) => {
    res.send(htmlTemplate(`http`, `navigate to /http/[status_code]`));
});

httpRouting.get('/:statusCode', (req, res) => {
    const statusCode = parseInt(req.params.statusCode, 10);
    const isJson = req.query.json !== undefined;

    if (isNaN(statusCode) || statusCode < 100 || statusCode > 599) {
        const errorMessage = 'Please provide a valid HTTP status code between 100 and 599.';
        
        if (isBrowser(req) && !isJson) {
            res.status(400).send(htmlTemplate('Invalid Status Code', errorMessage));
        } else {
            res.status(400).json({ error: errorMessage });
        }
        return;
    }

    const title = `Status Code: ${statusCode}`;
    const content = Object.entries(req.headers).map(([key, value]) => `${key}: ${value}`);

    if (isBrowser(req) && !isJson) {
        res.status(statusCode).send(htmlTemplate(title, content));
    } else {
        res.status(statusCode).json({ statusCode, header: content });
    }
});


// MARK: POST Requests
httpRouting.post('/:statusCode', (req, res) => {
    const statusCode = parseInt(req.params.statusCode, 10);
    const isJson = req.query.json !== undefined;

    if (isNaN(statusCode) || statusCode < 100 || statusCode > 599) {
        const errorMessage = 'Please provide a valid HTTP status code between 100 and 599.';
        
        if (isBrowser(req) && !isJson) {
            res.status(400).send(htmlTemplate('Invalid Status Code', errorMessage));
        } else {
            res.status(400).json({ error: errorMessage });
        }
        return;
    } else if (statusCode == 304) {
        if (isBrowser(req) && !isJson) {
            res.status(400).send(htmlTemplate('Error', 'Cannnot emulate a 304 request'));
        } else {
            res.status(400).json({ error: errorMessage });
        }
        return;
    }

    const title = `Status Code: ${statusCode}`;
    const content = Object.entries(req.headers).map(([key, value]) => `${key}: ${value}`);

    if (isBrowser(req) && !isJson) {
        res.status(statusCode).send(htmlTemplate(title, content));
    } else {
        res.status(statusCode).json({ statusCode, header: content });
    }
});


module.exports = httpRouting;