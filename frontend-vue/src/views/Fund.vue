<script setup>
import Button from "@/components/Button.vue"
import { NamiWalletApi, signAndSubmit } from "@/scripts/nami/nami.js"
import { Kuber } from "@/scripts/nami/kuber.js"
import { KuberJson } from "@/scripts/models/kuberJson.js"
import { market } from "@/config.js"
</script>

<template>
	<div class="container">
		<div class="glass p-4 center-window">
			<div class="col">
				<div class="form">
					<div class="mb-5">
						<div class="h2 text-center"> Let's Fund </div>
					</div>
					<div class="mb-4">
						<input id="money" class="form-control" name="money" placeholder="Ada">
					</div>
					<div class="text-center">
						<button @click="fund" class="btn btn-primary"> Fund </button>
					</div>
			</div>
			</div>
		</div>
	</div>
</template>

<script>
export default {
	name: "Fund",
	Components: {
		Button
	},
	methods: {
		async fund() {
			const money = document.getElementById("money").value
			if (money == 0) return
			const nami = new NamiWalletApi(window.cardano)
			const kuber = new Kuber()
			const kuberJson = new KuberJson()

			kuberJson.addSelectionAddrBech32(await nami.getAddress())
			kuberJson.addOutput(market.address, money + "A", {"constructor":0,"fields":[]})
			const data = kuberJson.getJsonString()

			const tx = await kuber.callKuber(data)
			console.log(tx.tx)
			console.log(tx.txHash)
			signAndSubmit(await window.cardano.nami.enable(), tx.tx)
		}
	}
}
</script>

<style scoped>
</style>
