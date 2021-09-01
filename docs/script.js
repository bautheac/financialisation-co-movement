let header = document.getElementById("header");
let title = document.querySelector(".title");
let authors = document.querySelectorAll(".author");
let date = document.querySelector(".date");
let abstract = document.querySelector(".abstract");

let container = document.createElement("div");
container.classList.add("authors");
authors.forEach(author => {
    console.log(author);
    container.appendChild(author);
});



header.innerHTML = "";
header.appendChild(title);
header.appendChild(container);
header.appendChild(date);
header.appendChild(abstract);

let link = document.getElementById("github");
link.classList.add("github");