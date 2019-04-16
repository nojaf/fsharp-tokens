export function getTokens(sourceCode) {
    return fetch("https://nojaf-functions.azurewebsites.net/api/GetTokens", {
        method: "POST",
        body: sourceCode
    })
    .then(response => response.text());
}