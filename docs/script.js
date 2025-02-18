import init, { MLtalk } from "./pkg/mltalk_wasm.js";

document.addEventListener("DOMContentLoaded", () =>
    init().then(() => {
        function Playground() {
            this.codeElm = document.getElementById("playground-code");
            this.resultElm = document.getElementById("playground-result");
            this.historyElm = document.getElementById("playground-history");
            this.historyList = [];
            this.historyIndex = 0;

            this.codeElm.addEventListener("keydown", (event) => {
                if (event.key == "Enter") {
                    this.eval();
                } else if (event.key == "ArrowDown") {
                    this.backHistory(1);
                } else if (event.key == "ArrowUp") {
                    this.backHistory(-1);
                } else if (event.key == "Escape") {
                    this.codeElm.value = "";
                }
            });

            this.eval = function () {
                let code = this.codeElm.value;
                let result = mltalkRuntime.eval(code);
                let is_fault = result.startsWith("Fault: ");
                let resultToShow = `
                        <p id="playground-result" style="overflow-wrap: break-word" class="notification code">
                            <span style="color: ${is_fault ? "red" : "green"} "> => </span>${result}
                        </p>
                    `.trim();
                this.historyList.push(code);
                this.historyIndex = this.historyList.length;

                let inputHist = document.createElement("p");
                inputHist.id = "playground-code";
                inputHist.className = "input is-light code";
                inputHist.textContent = code;

                this.codeElm.value = "";
                this.historyElm.appendChild(inputHist);
                this.historyElm.innerHTML += `${resultToShow}`;
                this.historyElm.scrollTo({
                    top: this.historyElm.scrollHeight,
                    behavior: "smooth",
                });

                urlParams.set("code", this.historyList.join("\n"));
                const newUrl = `${window.location.pathname}?${urlParams.toString()}`;
                history.pushState(null, "", newUrl);
            };

            this.backHistory = function (flag) {
                this.historyIndex += flag;
                let code = this.historyList[this.historyIndex];
                if (code != null && code != undefined) {
                    this.codeElm.value = code;
                } else {
                    this.historyIndex -= flag;
                }
                this.historyElm.children[this.historyIndex * 2].scrollIntoView({
                    behavior: "smooth",
                });
            };
        }

        let mltalkRuntime = new MLtalk();
        let playgroundObj = new Playground();
        let urlParams = new URLSearchParams(window.location.search);

        let sharedCode = urlParams.get("code");
        if (sharedCode != null) {
            sharedCode = sharedCode.trim();
            if (sharedCode != null && sharedCode != "") {
                sharedCode = decodeURIComponent(sharedCode);
                for (let line of sharedCode.split("\n")) {
                    playgroundObj.codeElm.value = line;
                    playgroundObj.eval();
                }
            }
        }
    }),
);
