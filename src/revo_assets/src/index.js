import { revo } from "../../declarations/revo";

document.getElementById("clickMeBtn").addEventListener("click", async () => {
  const name = document.getElementById("name").value.toString();
  // Interact with revo actor, calling the greet method
  const greeting = await revo.greet(name);

  document.getElementById("greeting").innerText = greeting;
});
