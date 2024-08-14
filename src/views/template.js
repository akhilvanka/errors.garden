module.exports = function htmlTemplate(title, content) {
    let contentHtml;

    if (Array.isArray(content)) {
        contentHtml = content.map(item => `<div class="headerRender">${item}</div>`).join('');
    } else {
        contentHtml = `<div>${content}</div>`;
    }

    return `
    <!DOCTYPE html>
    <html lang="en">

    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>errors.garden</title>
        <link rel="stylesheet" href="/css/styles.css">
    </head>

    <body>
        <main>
            <div>${title}</div>
            ${contentHtml}
        </main>
    </body>

    </html>
    `;
};
