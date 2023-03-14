<script setup>
import { NamiWalletApi, signAndSubmit } from "@/scripts/nami/nami.js"
import { BlockFrostApi } from "@/scripts/nami/blockfrost.js"
import { Kuber } from "@/scripts/nami/kuber.js"
import { KuberJson } from "@/scripts/models/kuberJson.js"
</script>

<template>
	<div @click="connectNami()" :class="[isConnected ? 'd-none' : '', 'd-flex', 'justify-content-end']" id="connect">
		<div class="btn btn-primary mt-3 me-4"> Connect </div>
	</div>
    <router-view></router-view>
</template>

<script>
export default {
  	name: 'App',
	data() {
		var isConnected = false
	},
	async created() {
		const nami = new NamiWalletApi(window.cardano)
		nami.enable()
		if (nami.isEnabled()) {
			this.isConnected = true
		}
	},
	methods: {
		async connectNami() {
			const nami = new NamiWalletApi(window.cardano, "api")
			nami.enable()
		}
	}
}
</script>
<style>
</style>
