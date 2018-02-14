import com.jetlore.deal_wizard.model.CampaignProductEvent
import com.jetlore.deal_wizard.profile.{ProfileTokenData, ProtoUserProfile}
import com.jetlore.tracking.core.model.TrackingEvent
import com.qwhispr.core.model.Attributes.MongoTokenData


object TestConverter {

  def serializeList[T](name : String, values : List[T]) = {
    values.zipWithIndex.map { case (value, index) => s"${name}_$index" -> value }.toMap
  }

  def jMapToMap[K, V](input : java.util.Map[K, V]) : Map[K, V] = ???

  def serializeMongoTokenData(mongoTokenData: Array[MongoTokenData]) : (Array[Int], Array[String]) = ???

  def serializeTokens( tokens : ProfileTokenData[MongoTokenData]) = {

    val (types, values) = serializeMongoTokenData(tokens.tokens)
    tokens.tokens.map((x: MongoTokenData) => x)

    Map(
      "tokens" -> Map("tokens_types" -> types, "token_values" -> values),
      // probably serialize ints somehow?
      "deal_token_map" -> jMapToMap(tokens.dealTokenMap).mapValues(_.mkString(";")),
      "opt_deal_sticky_token_map" -> jMapToMap(tokens.optDealStickyTokenMap).mapValues(_.mkString(";")),
      "opt_deal_non_sticky_token_map" -> jMapToMap(tokens.optDealNonStickyTokenMap).mapValues(_.mkString(";")),
      "search_history" -> tokens.searchHistory,
      "shopping_cart_history" -> tokens.shoppingCartHistory,
      "favorites_history" -> tokens.favoritesHistory,
      "wishlist_history" -> tokens.wishlistHistory,
      "purchases" -> tokens.purchases,
      "web_clicks" -> tokens.webClicks,
      "email_clicks" -> tokens.emailClicks,
      "bids" -> tokens.bids,
      "watches" -> tokens.watches,
      "browse_section_history" -> tokens.browseSectionHistory,
      "browse_promo_history" -> tokens.browsePromoHistory,
      "click_promo_history" -> tokens.clickPromoHistory,
      "browse_catalog_history" -> tokens.browseCatalogHistory
    )
  }

  def serializeTrackingEvent(event : TrackingEvent): Map[String, Any] = {
    Map(
      "feed_id"   -> event.feedId,
      "client_id" -> event.clientId,
      "user_id"   -> event.userId,

      "ctx_aciton_type"         -> event.ctx.actionType, //probably an extra field
      "ctx_source"              -> event.ctx.source,
      "ctx_feed_id"             -> event.ctx.feedId, //probably an extra field
      "ctx_client_id"           -> event.ctx.clientId.id, //probably an extra field
      "ctx_div"                 -> event.ctx.divLang.get.div,
      "ctx_lang"                -> event.ctx.divLang.get.lang,
      "ctx_campaign_id"         -> event.ctx.campaignId, //probably an extra field
      "ctx_user_id"             -> event.ctx.userId, //probably an extra field
      "ctx_timestamp"           -> event.ctx.timestamp,
      "ctx_add_to_user_profile" -> event.ctx.addToUserProfile, //probably an extra field

      "payload"             -> event.payload, // ???
      "action_type"         -> event.actionType,
      "source"              -> event.source,
      "feed_id"             -> event.feedId,
      "campaign_id"         -> event.campaignId,
      "timestamp"           -> event.timestamp,
      "add_to_user_profile" -> event.addToUserProfile
    )


  def seriallizeCampainProductEvent(event : CampaignProductEvent): Map[String, Any] = {
    Map(
      "campaign_id" -> event.campaignId,
      "feed_id" -> event.feedIdOpt,
      "product_id" -> event.productId,
      "time_stamp" -> event.timeStamp,
      "action_type" -> event.actionType,
      "position_index" -> event.position.get.itemIndex,
      "position_section" -> event.position.get.section
    )
  }

  def Convert(protoUserProfile : ProtoUserProfile[MongoTokenData]) = {

    val scalars: Map[String, Any] = Map(
      ("tpid", protoUserProfile.tpid),
      ("gender_male", protoUserProfile.gender.maleCount),
      ("gender_female", protoUserProfile.gender.maleCount),
      ("gender_unisex", protoUserProfile.gender.unisexCount),
      ("last_action_timestamp", protoUserProfile.lastActionTimestamp),
      ("last_build_timestamp", protoUserProfile.lastBuildTimestamp),
      ("impression_length", protoUserProfile.impressions.length ),
      ("promo_impression_length", protoUserProfile.promoImpressions.length )
      ("real_profile_length", protoUserProfile.promoImpressions.length )
    )

    val categoricalValueObserved: Map[String, Any] =
      protoUserProfile.categoricalValuesObserved.flatMap { case (k, v) => v.map { case (k2, v2) => s"$k:$k2" -> v2} }

    val categoricalValuesProvided: Map[String, Any] =
      protoUserProfile.categoricalValuesProvided.map{ case (k, v) => k.toString -> v }

    val main: Map[String, Map[String, Any]] = Map(
      "scalalrs" -> scalars,
      "categorical_values_observed" -> categoricalValueObserved,
      "categorical_values_provided" -> categoricalValuesProvided,
      "user_features" -> protoUserProfile.userFeatures
    )

    val result =
      main.toList ++
      serializeList("impressions", protoUserProfile.impressions.map(seriallizeCampainProductEvent)) ++
      serializeList("promo_impressions", protoUserProfile.promoImpressions.map(seriallizeCampainProductEvent)) ++
      serializeList("real_profile", protoUserProfile.realTimeExpiringProfile.events.map(serializeTrackingEvent))

    protoUserProfile.tokenData

    val id = protoUserProfile.tpid

    protoUserProfile.tokenData
  }
}}
