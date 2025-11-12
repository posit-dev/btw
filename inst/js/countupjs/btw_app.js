import { CountUp } from "./countUp.min.js";

const statusCounters = new WeakMap();

const statusPrefixes = {
  tokens_input: "&uparrow;",
  tokens_output: "&downarrow;",
  cost: "$",
};

function initializeCountUp(element, initialValue, options) {
  const statusType = element.dataset.type;
  const counter = new CountUp(element, initialValue, {
    duration: 1,
    prefix: statusPrefixes[statusType],
    separator: ",",
    decimal: ".",
    ...(options || {}),
  });
  return counter;
}

document.addEventListener("DOMContentLoaded", function () {
  document.querySelectorAll(".status-countup").forEach((element) => {
    const counter = initializeCountUp(element, 0);
    statusCounters.set(element, counter);
  });
});

if (typeof Shiny !== "undefined") {
  Shiny.addCustomMessageHandler("btw_update_status", function (message) {
    const element = document.getElementById(message.id);
    if (element) {
      const counter = statusCounters.get(element);
      const lastValue = parseFloat(element.dataset.value | "0");

      if (counter && message.value) {
        if (
          element.dataset.type === "cost" &&
          (lastValue < 0.1 || message.value < 0.1)
        ) {
          const newCounter = initializeCountUp(element, lastValue, {
            decimalPlaces:
              message.value < 0.01 ? 4 : message.value < 0.1 ? 3 : 2,
          });
          statusCounters.set(element, newCounter);
          newCounter.update(message.value);
        } else {
          counter.update(message.value);
        }
      }
      element.dataset.value = message.value;

      element.classList.toggle(
        "btw-status-recalculating",
        message.status === "recalculating"
      );

      if (message.status === "unknown") {
        element.classList.add("btw-status-unknown");
      }
    }
  });
}
