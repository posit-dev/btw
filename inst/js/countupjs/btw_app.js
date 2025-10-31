import { CountUp } from './countUp.min.js';

const statusCounters = new WeakMap();

const statusPrefixes = {
  'tokens_input': '&uparrow;',
  'tokens_output': '&downarrow;',
  'cost': '$'
}

document.addEventListener('DOMContentLoaded', function() {
  document.querySelectorAll(".status-countup").forEach(element => {
    const statusType = element.dataset.type;
    const counter = new CountUp(element, 0, {
      duration: 1,
      prefix: statusPrefixes[statusType],
      separator: ',',
      decimal: '.',
      decimalPlaces: statusType === 'cost' ? 2 : 0,
    });
    statusCounters.set(element, counter);
  })
});

if (typeof Shiny !== 'undefined') {
  Shiny.addCustomMessageHandler('btw_update_status', function(message) {
    const element = document.getElementById(message.id);
    if (element) {
      const counter = statusCounters.get(element);
      if (counter && message.value) {
        counter.update(message.value);
      }

      element.classList.toggle('btw-status-recalculating', message.status === 'recalculating');

      if (message.status === 'unknown') {
        element.classList.add('btw-status-unknown');
      }
    }
  });
}
