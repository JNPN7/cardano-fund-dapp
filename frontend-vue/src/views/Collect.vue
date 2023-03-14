<script setup>
import { NamiWalletApi, signAndSubmit } from "@/scripts/nami/nami.js"
import { Kuber } from "@/scripts/nami/kuber.js"
import { KuberJson } from "@/scripts/models/kuberJson.js"
import { market, beneficiary } from "@/config.js"
import { BlockFrostApi } from "@/scripts/nami/blockfrost.js"
</script>

<template>
	<div class="container">
		<div class="glass p-4 center-window">
			<div class="col">
				<div class="form">
					<div class="mb-5">
						<div class="h2 text-center"> Let's Collect </div>
					</div>
					<div class="text-center">
						<button @click="collect" class="btn btn-primary"> Collect </button>
					</div>
				</div>

			</div>
		</div>
	</div>
</template>

<script>
export default {
	name: "Collect",
	methods: {
		async collect() {
			const nami = new NamiWalletApi(window.cardano)
			const kuber = new Kuber()
			const kuberJson = new KuberJson()
			const blockfrost = new BlockFrostApi(nami)

			var balance = await blockfrost.getScriptBalance()
			var utxos = await blockfrost.getUtxos()

			kuberJson.addSelectionAddrBech32(await nami.getAddress())
			kuberJson.addOutput(beneficiary.beneficiary1, balance/2 + "lovelace")
			kuberJson.addOutput(beneficiary.beneficiary2, balance/2 + "lovelace")
			//kuberJson.addInputsScript()
			utxos.forEach(utxo => {
				kuberJson.addInputsScript(
					utxo.tx_hash,
					utxo.tx_index,
					market.script.type,
					market.script.description,
					market.script.cborHex,
				)
			})
			kuberJson.addCollateralCborHex(await window.cardano.getCollateral())
			const data = kuberJson.getJsonString()
			console.log(data)

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
