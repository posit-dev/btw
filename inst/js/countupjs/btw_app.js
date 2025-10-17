import { CountUp } from './countUp.min.js';

const statusCounters = new WeakMap();

const statusPrefixes = {
  'status_tokens_input': '&uparrow;',
  'status_tokens_output': '&downarrow;',
  'status_cost': '$'
}

document.addEventListener('DOMContentLoaded', function() {
  ['status_tokens_input', 'status_tokens_output', 'status_cost'].forEach(id => {
    const element = document.getElementById(id);
    if (element) {
      const counter = new CountUp(element, 0, {
        duration: 1,
        prefix: statusPrefixes[id],
        separator: ',',
        decimal: '.',
        decimalPlaces: id === 'status_cost' ? 2 : 0,
      });
      statusCounters.set(element, counter);
    }
  });
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
