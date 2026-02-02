%%% @author Konstantin Rusalov
%%% @copyright (c) 2026 Konstantin Rusalov
-module(telegram_bot_api).
-moduledoc """
Telegram Bot API.
""".

-export_type([update_type/0, pool_name/0, secret_token/0, ret/0]).

-doc """
File description for a multipart/form-data request.
This type is used when sending files via the HTTP API.
Fields:
- `file` — the absolute path to the file on the file system
- `name` — the file name (basename) that will be passed to the server
""".
-type multipart_file() :: #{file := binary(), name := binary()}.

-type empty_list() :: [].
-type response_error() :: #{
    ok := false,
    description := binary(),
    error_code := integer(),
    parameters => 'ResponseParameters'()
}.
-type response_ok(Type) :: #{ok := true, result := Type | empty_list(), description => binary()}.

-type http_code() :: 200 | 400 | 401 | 500 | pos_integer().
-type error() :: {error, term()} | {Reason :: term(), Exception :: term()}.

-type async() :: {ok, Ref :: reference()}.

-type ret() :: async() | error().

-type result(Type) ::
    {ok, HttpCode :: http_code(), Json :: response_ok(Type) | response_error()} | ret().

-type pool_name() :: atom().
-type empty_map() :: #{}.

-doc """
See `t:'Update'/0`
""".
-type 'update_type'() ::
    message
    | edited_message
    | channel_post
    | edited_channel_post
    | inline_query
    | chosen_inline_result
    | callback_query
    | custom_event
    | custom_query
    | shipping_query
    | pre_checkout_query
    | poll
    | poll_answer
    | my_chat_member
    | chat_member
    | chat_join_request
    | chat_boost
    | removed_chat_boost
    | message_reaction
    | message_reaction_count
    | business_connection
    | business_message
    | edited_business_message
    | deleted_business_messages
    | purchased_paid_media.

-doc """
A secret token to be sent in a header “X-Telegram-Bot-Api-Secret-Token” in every webhook request, 1-256 characters. Only characters A-Z, a-z, 0-9, _ and - are allowed. The header is useful to ensure that the request comes from a webhook set by you.
""".
-type secret_token() :: nonempty_binary().

-export([
	getUpdates/2, getUpdates/3,
	setWebhook/2, setWebhook/3,
	deleteWebhook/2, deleteWebhook/3,
	logOut/2, logOut/3,
	close/2, close/3,
	sendMessageDraft/2, sendMessageDraft/3,
	sendChatAction/2, sendChatAction/3,
	setMessageReaction/2, setMessageReaction/3,
	setUserEmojiStatus/2, setUserEmojiStatus/3,
	banChatMember/2, banChatMember/3,
	unbanChatMember/2, unbanChatMember/3,
	restrictChatMember/2, restrictChatMember/3,
	promoteChatMember/2, promoteChatMember/3,
	setChatAdministratorCustomTitle/2, setChatAdministratorCustomTitle/3,
	banChatSenderChat/2, banChatSenderChat/3,
	unbanChatSenderChat/2, unbanChatSenderChat/3,
	setChatPermissions/2, setChatPermissions/3,
	approveChatJoinRequest/2, approveChatJoinRequest/3,
	declineChatJoinRequest/2, declineChatJoinRequest/3,
	setChatPhoto/2, setChatPhoto/3,
	deleteChatPhoto/2, deleteChatPhoto/3,
	setChatTitle/2, setChatTitle/3,
	setChatDescription/2, setChatDescription/3,
	pinChatMessage/2, pinChatMessage/3,
	unpinChatMessage/2, unpinChatMessage/3,
	unpinAllChatMessages/2, unpinAllChatMessages/3,
	leaveChat/2, leaveChat/3,
	setChatStickerSet/2, setChatStickerSet/3,
	deleteChatStickerSet/2, deleteChatStickerSet/3,
	editForumTopic/2, editForumTopic/3,
	closeForumTopic/2, closeForumTopic/3,
	reopenForumTopic/2, reopenForumTopic/3,
	deleteForumTopic/2, deleteForumTopic/3,
	unpinAllForumTopicMessages/2, unpinAllForumTopicMessages/3,
	editGeneralForumTopic/2, editGeneralForumTopic/3,
	closeGeneralForumTopic/2, closeGeneralForumTopic/3,
	reopenGeneralForumTopic/2, reopenGeneralForumTopic/3,
	hideGeneralForumTopic/2, hideGeneralForumTopic/3,
	unhideGeneralForumTopic/2, unhideGeneralForumTopic/3,
	unpinAllGeneralForumTopicMessages/2, unpinAllGeneralForumTopicMessages/3,
	answerCallbackQuery/2, answerCallbackQuery/3,
	setMyCommands/2, setMyCommands/3,
	deleteMyCommands/2, deleteMyCommands/3,
	setMyName/2, setMyName/3,
	setMyDescription/2, setMyDescription/3,
	setMyShortDescription/2, setMyShortDescription/3,
	setChatMenuButton/2, setChatMenuButton/3,
	setMyDefaultAdministratorRights/2, setMyDefaultAdministratorRights/3,
	sendGift/2, sendGift/3,
	giftPremiumSubscription/2, giftPremiumSubscription/3,
	verifyUser/2, verifyUser/3,
	verifyChat/2, verifyChat/3,
	removeUserVerification/2, removeUserVerification/3,
	removeChatVerification/2, removeChatVerification/3,
	readBusinessMessage/2, readBusinessMessage/3,
	deleteBusinessMessages/2, deleteBusinessMessages/3,
	setBusinessAccountName/2, setBusinessAccountName/3,
	setBusinessAccountUsername/2, setBusinessAccountUsername/3,
	setBusinessAccountBio/2, setBusinessAccountBio/3,
	setBusinessAccountProfilePhoto/2, setBusinessAccountProfilePhoto/3,
	removeBusinessAccountProfilePhoto/2, removeBusinessAccountProfilePhoto/3,
	setBusinessAccountGiftSettings/2, setBusinessAccountGiftSettings/3,
	transferBusinessAccountStars/2, transferBusinessAccountStars/3,
	convertGiftToStars/2, convertGiftToStars/3,
	upgradeGift/2, upgradeGift/3,
	transferGift/2, transferGift/3,
	deleteStory/2, deleteStory/3,
	approveSuggestedPost/2, approveSuggestedPost/3,
	declineSuggestedPost/2, declineSuggestedPost/3,
	deleteMessage/2, deleteMessage/3,
	deleteMessages/2, deleteMessages/3,
	createNewStickerSet/2, createNewStickerSet/3,
	addStickerToSet/2, addStickerToSet/3,
	setStickerPositionInSet/2, setStickerPositionInSet/3,
	deleteStickerFromSet/2, deleteStickerFromSet/3,
	replaceStickerInSet/2, replaceStickerInSet/3,
	setStickerEmojiList/2, setStickerEmojiList/3,
	setStickerKeywords/2, setStickerKeywords/3,
	setStickerMaskPosition/2, setStickerMaskPosition/3,
	setStickerSetTitle/2, setStickerSetTitle/3,
	setStickerSetThumbnail/2, setStickerSetThumbnail/3,
	setCustomEmojiStickerSetThumbnail/2, setCustomEmojiStickerSetThumbnail/3,
	deleteStickerSet/2, deleteStickerSet/3,
	answerInlineQuery/2, answerInlineQuery/3,
	answerShippingQuery/2, answerShippingQuery/3,
	answerPreCheckoutQuery/2, answerPreCheckoutQuery/3,
	refundStarPayment/2, refundStarPayment/3,
	editUserStarSubscription/2, editUserStarSubscription/3,
	setPassportDataErrors/2, setPassportDataErrors/3,
	getWebhookInfo/2, getWebhookInfo/3,
	getMe/2, getMe/3,
	sendMessage/2, sendMessage/3,
	forwardMessage/2, forwardMessage/3,
	sendPhoto/2, sendPhoto/3,
	sendAudio/2, sendAudio/3,
	sendDocument/2, sendDocument/3,
	sendVideo/2, sendVideo/3,
	sendAnimation/2, sendAnimation/3,
	sendVoice/2, sendVoice/3,
	sendVideoNote/2, sendVideoNote/3,
	sendPaidMedia/2, sendPaidMedia/3,
	sendLocation/2, sendLocation/3,
	sendVenue/2, sendVenue/3,
	sendContact/2, sendContact/3,
	sendPoll/2, sendPoll/3,
	sendChecklist/2, sendChecklist/3,
	sendDice/2, sendDice/3,
	editMessageChecklist/2, editMessageChecklist/3,
	sendSticker/2, sendSticker/3,
	sendInvoice/2, sendInvoice/3,
	sendGame/2, sendGame/3,
	forwardMessages/2, forwardMessages/3,
	copyMessages/2, copyMessages/3,
	copyMessage/2, copyMessage/3,
	sendMediaGroup/2, sendMediaGroup/3,
	getUserProfilePhotos/2, getUserProfilePhotos/3,
	getFile/2, getFile/3,
	uploadStickerFile/2, uploadStickerFile/3,
	exportChatInviteLink/2, exportChatInviteLink/3,
	createInvoiceLink/2, createInvoiceLink/3,
	createChatInviteLink/2, createChatInviteLink/3,
	editChatInviteLink/2, editChatInviteLink/3,
	createChatSubscriptionInviteLink/2, createChatSubscriptionInviteLink/3,
	editChatSubscriptionInviteLink/2, editChatSubscriptionInviteLink/3,
	revokeChatInviteLink/2, revokeChatInviteLink/3,
	getChat/2, getChat/3,
	getChatAdministrators/2, getChatAdministrators/3,
	getChatMemberCount/2, getChatMemberCount/3,
	getChatMember/2, getChatMember/3,
	getForumTopicIconStickers/2, getForumTopicIconStickers/3,
	getCustomEmojiStickers/2, getCustomEmojiStickers/3,
	createForumTopic/2, createForumTopic/3,
	getUserChatBoosts/2, getUserChatBoosts/3,
	getBusinessConnection/2, getBusinessConnection/3,
	getMyCommands/2, getMyCommands/3,
	getMyName/2, getMyName/3,
	getMyDescription/2, getMyDescription/3,
	getMyShortDescription/2, getMyShortDescription/3,
	getChatMenuButton/2, getChatMenuButton/3,
	getMyDefaultAdministratorRights/2, getMyDefaultAdministratorRights/3,
	getAvailableGifts/2, getAvailableGifts/3,
	getBusinessAccountStarBalance/2, getBusinessAccountStarBalance/3,
	getMyStarBalance/2, getMyStarBalance/3,
	getBusinessAccountGifts/2, getBusinessAccountGifts/3,
	getUserGifts/2, getUserGifts/3,
	getChatGifts/2, getChatGifts/3,
	postStory/2, postStory/3,
	repostStory/2, repostStory/3,
	editStory/2, editStory/3,
	editMessageText/2, editMessageText/3,
	editMessageCaption/2, editMessageCaption/3,
	editMessageMedia/2, editMessageMedia/3,
	editMessageLiveLocation/2, editMessageLiveLocation/3,
	stopMessageLiveLocation/2, stopMessageLiveLocation/3,
	editMessageReplyMarkup/2, editMessageReplyMarkup/3,
	setGameScore/2, setGameScore/3,
	stopPoll/2, stopPoll/3,
	getStickerSet/2, getStickerSet/3,
	answerWebAppQuery/2, answerWebAppQuery/3,
	savePreparedInlineMessage/2, savePreparedInlineMessage/3,
	getStarTransactions/2, getStarTransactions/3,
	getGameHighScores/2, getGameHighScores/3
]).



-doc """
This object represents an incoming update.  
At most one of the optional parameters can be present in any given update.
  * `update_id` - The update's unique identifier. Update identifiers start from a certain positive number and increase sequentially. This identifier becomes especially handy if you're using webhooks, since it allows you to ignore repeated updates or to restore the correct update sequence, should they get out of order. If there are no new updates for at least a week, then identifier of the next update will be chosen randomly instead of sequentially.
  * `message` - Optional. New incoming message of any kind - text, photo, sticker, etc.
  * `edited_message` - Optional. New version of a message that is known to the bot and was edited. This update may at times be triggered by changes to message fields that are either unavailable or not actively used by your bot.
  * `channel_post` - Optional. New incoming channel post of any kind - text, photo, sticker, etc.
  * `edited_channel_post` - Optional. New version of a channel post that is known to the bot and was edited. This update may at times be triggered by changes to message fields that are either unavailable or not actively used by your bot.
  * `business_connection` - Optional. The bot was connected to or disconnected from a business account, or a user edited an existing connection with the bot
  * `business_message` - Optional. New message from a connected business account
  * `edited_business_message` - Optional. New version of a message from a connected business account
  * `deleted_business_messages` - Optional. Messages were deleted from a connected business account
  * `message_reaction` - Optional. A reaction to a message was changed by a user. The bot must be an administrator in the chat and must explicitly specify message_reaction in the list of allowed_updates to receive these updates. The update isn't received for reactions set by bots.
  * `message_reaction_count` - Optional. Reactions to a message with anonymous reactions were changed. The bot must be an administrator in the chat and must explicitly specify message_reaction_count in the list of allowed_updates to receive these updates. The updates are grouped and can be sent with delay up to a few minutes.
  * `inline_query` - Optional. New incoming inline query
  * `chosen_inline_result` - Optional. The result of an inline query that was chosen by a user and sent to their chat partner. Please see our documentation on the feedback collecting for details on how to enable these updates for your bot.
  * `callback_query` - Optional. New incoming callback query
  * `shipping_query` - Optional. New incoming shipping query. Only for invoices with flexible price
  * `pre_checkout_query` - Optional. New incoming pre-checkout query. Contains full information about checkout
  * `purchased_paid_media` - Optional. A user purchased paid media with a non-empty payload sent by the bot in a non-channel chat
  * `poll` - Optional. New poll state. Bots receive only updates about manually stopped polls and polls, which are sent by the bot
  * `poll_answer` - Optional. A user changed their answer in a non-anonymous poll. Bots receive new votes only in polls that were sent by the bot itself.
  * `my_chat_member` - Optional. The bot's chat member status was updated in a chat. For private chats, this update is received only when the bot is blocked or unblocked by the user.
  * `chat_member` - Optional. A chat member's status was updated in a chat. The bot must be an administrator in the chat and must explicitly specify chat_member in the list of allowed_updates to receive these updates.
  * `chat_join_request` - Optional. A request to join the chat has been sent. The bot must have the can_invite_users administrator right in the chat to receive these updates.
  * `chat_boost` - Optional. A chat boost was added or changed. The bot must be an administrator in the chat to receive these updates.
  * `removed_chat_boost` - Optional. A boost was removed from a chat. The bot must be an administrator in the chat to receive these updates.
""".
-type 'Update'() :: #{
	update_id := integer(),
	message => 'Message'(),
	edited_message => 'Message'(),
	channel_post => 'Message'(),
	edited_channel_post => 'Message'(),
	business_connection => 'BusinessConnection'(),
	business_message => 'Message'(),
	edited_business_message => 'Message'(),
	deleted_business_messages => 'BusinessMessagesDeleted'(),
	message_reaction => 'MessageReactionUpdated'(),
	message_reaction_count => 'MessageReactionCountUpdated'(),
	inline_query => 'InlineQuery'(),
	chosen_inline_result => 'ChosenInlineResult'(),
	callback_query => 'CallbackQuery'(),
	shipping_query => 'ShippingQuery'(),
	pre_checkout_query => 'PreCheckoutQuery'(),
	purchased_paid_media => 'PaidMediaPurchased'(),
	poll => 'Poll'(),
	poll_answer => 'PollAnswer'(),
	my_chat_member => 'ChatMemberUpdated'(),
	chat_member => 'ChatMemberUpdated'(),
	chat_join_request => 'ChatJoinRequest'(),
	chat_boost => 'ChatBoostUpdated'(),
	removed_chat_boost => 'ChatBoostRemoved'()
}.

-doc """
Describes the current status of a webhook.
  * `url` - Webhook URL, may be empty if webhook is not set up
  * `has_custom_certificate` - True, if a custom certificate was provided for webhook certificate checks
  * `pending_update_count` - Number of updates awaiting delivery
  * `ip_address` - Optional. Currently used webhook IP address
  * `last_error_date` - Optional. Unix time for the most recent error that happened when trying to deliver an update via webhook
  * `last_error_message` - Optional. Error message in human-readable format for the most recent error that happened when trying to deliver an update via webhook
  * `last_synchronization_error_date` - Optional. Unix time of the most recent error that happened when trying to synchronize available updates with Telegram datacenters
  * `max_connections` - Optional. The maximum allowed number of simultaneous HTTPS connections to the webhook for update delivery
  * `allowed_updates` - Optional. A list of update types the bot is subscribed to. Defaults to all update types except chat_member
""".
-type 'WebhookInfo'() :: #{
	url := binary(),
	has_custom_certificate := boolean(),
	pending_update_count := integer(),
	ip_address => binary(),
	last_error_date => integer(),
	last_error_message => binary(),
	last_synchronization_error_date => integer(),
	max_connections => integer(),
	allowed_updates => nonempty_list(binary())
}.

-doc """
This object represents a Telegram user or bot.
  * `id` - Unique identifier for this user or bot. This number may have more than 32 significant bits and some programming languages may have difficulty/silent defects in interpreting it. But it has at most 52 significant bits, so a 64-bit integer or double-precision float type are safe for storing this identifier.
  * `is_bot` - True, if this user is a bot
  * `first_name` - User's or bot's first name
  * `last_name` - Optional. User's or bot's last name
  * `username` - Optional. User's or bot's username
  * `language_code` - Optional. IETF language tag of the user's language
  * `is_premium` - Optional. True, if this user is a Telegram Premium user
  * `added_to_attachment_menu` - Optional. True, if this user added the bot to the attachment menu
  * `can_join_groups` - Optional. True, if the bot can be invited to groups. Returned only in getMe.
  * `can_read_all_group_messages` - Optional. True, if privacy mode is disabled for the bot. Returned only in getMe.
  * `supports_inline_queries` - Optional. True, if the bot supports inline queries. Returned only in getMe.
  * `can_connect_to_business` - Optional. True, if the bot can be connected to a Telegram Business account to receive its messages. Returned only in getMe.
  * `has_main_web_app` - Optional. True, if the bot has a main Web App. Returned only in getMe.
  * `has_topics_enabled` - Optional. True, if the bot has forum topic mode enabled in private chats. Returned only in getMe.
""".
-type 'User'() :: #{
	id := integer(),
	is_bot := boolean(),
	first_name := binary(),
	last_name => binary(),
	username => binary(),
	language_code => binary(),
	is_premium => true,
	added_to_attachment_menu => true,
	can_join_groups => boolean(),
	can_read_all_group_messages => boolean(),
	supports_inline_queries => boolean(),
	can_connect_to_business => boolean(),
	has_main_web_app => boolean(),
	has_topics_enabled => boolean()
}.

-doc """
This object represents a chat.
  * `id` - Unique identifier for this chat. This number may have more than 32 significant bits and some programming languages may have difficulty/silent defects in interpreting it. But it has at most 52 significant bits, so a signed 64-bit integer or double-precision float type are safe for storing this identifier.
  * `type` - Type of the chat, can be either “private”, “group”, “supergroup” or “channel”
  * `title` - Optional. Title, for supergroups, channels and group chats
  * `username` - Optional. Username, for private chats, supergroups and channels if available
  * `first_name` - Optional. First name of the other party in a private chat
  * `last_name` - Optional. Last name of the other party in a private chat
  * `is_forum` - Optional. True, if the supergroup chat is a forum (has topics enabled)
  * `is_direct_messages` - Optional. True, if the chat is the direct messages chat of a channel
""".
-type 'Chat'() :: #{
	id := integer(),
	type := binary(),
	title => binary(),
	username => binary(),
	first_name => binary(),
	last_name => binary(),
	is_forum => true,
	is_direct_messages => true
}.

-doc """
This object contains full information about a chat.
  * `id` - Unique identifier for this chat. This number may have more than 32 significant bits and some programming languages may have difficulty/silent defects in interpreting it. But it has at most 52 significant bits, so a signed 64-bit integer or double-precision float type are safe for storing this identifier.
  * `type` - Type of the chat, can be either “private”, “group”, “supergroup” or “channel”
  * `title` - Optional. Title, for supergroups, channels and group chats
  * `username` - Optional. Username, for private chats, supergroups and channels if available
  * `first_name` - Optional. First name of the other party in a private chat
  * `last_name` - Optional. Last name of the other party in a private chat
  * `is_forum` - Optional. True, if the supergroup chat is a forum (has topics enabled)
  * `is_direct_messages` - Optional. True, if the chat is the direct messages chat of a channel
  * `accent_color_id` - Identifier of the accent color for the chat name and backgrounds of the chat photo, reply header, and link preview. See accent colors for more details.
  * `max_reaction_count` - The maximum number of reactions that can be set on a message in the chat
  * `photo` - Optional. Chat photo
  * `active_usernames` - Optional. If non-empty, the list of all active chat usernames; for private chats, supergroups and channels
  * `birthdate` - Optional. For private chats, the date of birth of the user
  * `business_intro` - Optional. For private chats with business accounts, the intro of the business
  * `business_location` - Optional. For private chats with business accounts, the location of the business
  * `business_opening_hours` - Optional. For private chats with business accounts, the opening hours of the business
  * `personal_chat` - Optional. For private chats, the personal channel of the user
  * `parent_chat` - Optional. Information about the corresponding channel chat; for direct messages chats only
  * `available_reactions` - Optional. List of available reactions allowed in the chat. If omitted, then all emoji reactions are allowed.
  * `background_custom_emoji_id` - Optional. Custom emoji identifier of the emoji chosen by the chat for the reply header and link preview background
  * `profile_accent_color_id` - Optional. Identifier of the accent color for the chat's profile background. See profile accent colors for more details.
  * `profile_background_custom_emoji_id` - Optional. Custom emoji identifier of the emoji chosen by the chat for its profile background
  * `emoji_status_custom_emoji_id` - Optional. Custom emoji identifier of the emoji status of the chat or the other party in a private chat
  * `emoji_status_expiration_date` - Optional. Expiration date of the emoji status of the chat or the other party in a private chat, in Unix time, if any
  * `bio` - Optional. Bio of the other party in a private chat
  * `has_private_forwards` - Optional. True, if privacy settings of the other party in the private chat allows to use tg://user?id=<user_id> links only in chats with the user
  * `has_restricted_voice_and_video_messages` - Optional. True, if the privacy settings of the other party restrict sending voice and video note messages in the private chat
  * `join_to_send_messages` - Optional. True, if users need to join the supergroup before they can send messages
  * `join_by_request` - Optional. True, if all users directly joining the supergroup without using an invite link need to be approved by supergroup administrators
  * `description` - Optional. Description, for groups, supergroups and channel chats
  * `invite_link` - Optional. Primary invite link, for groups, supergroups and channel chats
  * `pinned_message` - Optional. The most recent pinned message (by sending date)
  * `permissions` - Optional. Default chat member permissions, for groups and supergroups
  * `accepted_gift_types` - Information about types of gifts that are accepted by the chat or by the corresponding user for private chats
  * `can_send_paid_media` - Optional. True, if paid media messages can be sent or forwarded to the channel chat. The field is available only for channel chats.
  * `slow_mode_delay` - Optional. For supergroups, the minimum allowed delay between consecutive messages sent by each unprivileged user; in seconds
  * `unrestrict_boost_count` - Optional. For supergroups, the minimum number of boosts that a non-administrator user needs to add in order to ignore slow mode and chat permissions
  * `message_auto_delete_time` - Optional. The time after which all messages sent to the chat will be automatically deleted; in seconds
  * `has_aggressive_anti_spam_enabled` - Optional. True, if aggressive anti-spam checks are enabled in the supergroup. The field is only available to chat administrators.
  * `has_hidden_members` - Optional. True, if non-administrators can only get the list of bots and administrators in the chat
  * `has_protected_content` - Optional. True, if messages from the chat can't be forwarded to other chats
  * `has_visible_history` - Optional. True, if new chat members will have access to old messages; available only to chat administrators
  * `sticker_set_name` - Optional. For supergroups, name of the group sticker set
  * `can_set_sticker_set` - Optional. True, if the bot can change the group sticker set
  * `custom_emoji_sticker_set_name` - Optional. For supergroups, the name of the group's custom emoji sticker set. Custom emoji from this set can be used by all users and bots in the group.
  * `linked_chat_id` - Optional. Unique identifier for the linked chat, i.e. the discussion group identifier for a channel and vice versa; for supergroups and channel chats. This identifier may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision float type are safe for storing this identifier.
  * `location` - Optional. For supergroups, the location to which the supergroup is connected
  * `rating` - Optional. For private chats, the rating of the user if any
  * `unique_gift_colors` - Optional. The color scheme based on a unique gift that must be used for the chat's name, message replies and link previews
  * `paid_message_star_count` - Optional. The number of Telegram Stars a general user have to pay to send a message to the chat
""".
-type 'ChatFullInfo'() :: #{
	id := integer(),
	type := binary(),
	title => binary(),
	username => binary(),
	first_name => binary(),
	last_name => binary(),
	is_forum => true,
	is_direct_messages => true,
	accent_color_id := integer(),
	max_reaction_count := integer(),
	photo => 'ChatPhoto'(),
	active_usernames => nonempty_list(binary()),
	birthdate => 'Birthdate'(),
	business_intro => 'BusinessIntro'(),
	business_location => 'BusinessLocation'(),
	business_opening_hours => 'BusinessOpeningHours'(),
	personal_chat => 'Chat'(),
	parent_chat => 'Chat'(),
	available_reactions => nonempty_list('ReactionType'()),
	background_custom_emoji_id => binary(),
	profile_accent_color_id => integer(),
	profile_background_custom_emoji_id => binary(),
	emoji_status_custom_emoji_id => binary(),
	emoji_status_expiration_date => integer(),
	bio => binary(),
	has_private_forwards => true,
	has_restricted_voice_and_video_messages => true,
	join_to_send_messages => true,
	join_by_request => true,
	description => binary(),
	invite_link => binary(),
	pinned_message => 'Message'(),
	permissions => 'ChatPermissions'(),
	accepted_gift_types := 'AcceptedGiftTypes'(),
	can_send_paid_media => true,
	slow_mode_delay => integer(),
	unrestrict_boost_count => integer(),
	message_auto_delete_time => integer(),
	has_aggressive_anti_spam_enabled => true,
	has_hidden_members => true,
	has_protected_content => true,
	has_visible_history => true,
	sticker_set_name => binary(),
	can_set_sticker_set => true,
	custom_emoji_sticker_set_name => binary(),
	linked_chat_id => integer(),
	location => 'ChatLocation'(),
	rating => 'UserRating'(),
	unique_gift_colors => 'UniqueGiftColors'(),
	paid_message_star_count => integer()
}.

-doc """
This object represents a message.
  * `message_id` - Unique message identifier inside this chat. In specific instances (e.g., message containing a video sent to a big chat), the server might automatically schedule a message instead of sending it immediately. In such cases, this field will be 0 and the relevant message will be unusable until it is actually sent
  * `message_thread_id` - Optional. Unique identifier of a message thread or forum topic to which the message belongs; for supergroups and private chats only
  * `direct_messages_topic` - Optional. Information about the direct messages chat topic that contains the message
  * `from` - Optional. Sender of the message; may be empty for messages sent to channels. For backward compatibility, if the message was sent on behalf of a chat, the field contains a fake sender user in non-channel chats
  * `sender_chat` - Optional. Sender of the message when sent on behalf of a chat. For example, the supergroup itself for messages sent by its anonymous administrators or a linked channel for messages automatically forwarded to the channel's discussion group. For backward compatibility, if the message was sent on behalf of a chat, the field from contains a fake sender user in non-channel chats.
  * `sender_boost_count` - Optional. If the sender of the message boosted the chat, the number of boosts added by the user
  * `sender_business_bot` - Optional. The bot that actually sent the message on behalf of the business account. Available only for outgoing messages sent on behalf of the connected business account.
  * `date` - Date the message was sent in Unix time. It is always a positive number, representing a valid date.
  * `business_connection_id` - Optional. Unique identifier of the business connection from which the message was received. If non-empty, the message belongs to a chat of the corresponding business account that is independent from any potential bot chat which might share the same identifier.
  * `chat` - Chat the message belongs to
  * `forward_origin` - Optional. Information about the original message for forwarded messages
  * `is_topic_message` - Optional. True, if the message is sent to a topic in a forum supergroup or a private chat with the bot
  * `is_automatic_forward` - Optional. True, if the message is a channel post that was automatically forwarded to the connected discussion group
  * `reply_to_message` - Optional. For replies in the same chat and message thread, the original message. Note that the Message object in this field will not contain further reply_to_message fields even if it itself is a reply.
  * `external_reply` - Optional. Information about the message that is being replied to, which may come from another chat or forum topic
  * `quote` - Optional. For replies that quote part of the original message, the quoted part of the message
  * `reply_to_story` - Optional. For replies to a story, the original story
  * `reply_to_checklist_task_id` - Optional. Identifier of the specific checklist task that is being replied to
  * `via_bot` - Optional. Bot through which the message was sent
  * `edit_date` - Optional. Date the message was last edited in Unix time
  * `has_protected_content` - Optional. True, if the message can't be forwarded
  * `is_from_offline` - Optional. True, if the message was sent by an implicit action, for example, as an away or a greeting business message, or as a scheduled message
  * `is_paid_post` - Optional. True, if the message is a paid post. Note that such posts must not be deleted for 24 hours to receive the payment and can't be edited.
  * `media_group_id` - Optional. The unique identifier of a media message group this message belongs to
  * `author_signature` - Optional. Signature of the post author for messages in channels, or the custom title of an anonymous group administrator
  * `paid_star_count` - Optional. The number of Telegram Stars that were paid by the sender of the message to send it
  * `text` - Optional. For text messages, the actual UTF-8 text of the message
  * `entities` - Optional. For text messages, special entities like usernames, URLs, bot commands, etc. that appear in the text
  * `link_preview_options` - Optional. Options used for link preview generation for the message, if it is a text message and link preview options were changed
  * `suggested_post_info` - Optional. Information about suggested post parameters if the message is a suggested post in a channel direct messages chat. If the message is an approved or declined suggested post, then it can't be edited.
  * `effect_id` - Optional. Unique identifier of the message effect added to the message
  * `animation` - Optional. Message is an animation, information about the animation. For backward compatibility, when this field is set, the document field will also be set
  * `audio` - Optional. Message is an audio file, information about the file
  * `document` - Optional. Message is a general file, information about the file
  * `paid_media` - Optional. Message contains paid media; information about the paid media
  * `photo` - Optional. Message is a photo, available sizes of the photo
  * `sticker` - Optional. Message is a sticker, information about the sticker
  * `story` - Optional. Message is a forwarded story
  * `video` - Optional. Message is a video, information about the video
  * `video_note` - Optional. Message is a video note, information about the video message
  * `voice` - Optional. Message is a voice message, information about the file
  * `caption` - Optional. Caption for the animation, audio, document, paid media, photo, video or voice
  * `caption_entities` - Optional. For messages with a caption, special entities like usernames, URLs, bot commands, etc. that appear in the caption
  * `show_caption_above_media` - Optional. True, if the caption must be shown above the message media
  * `has_media_spoiler` - Optional. True, if the message media is covered by a spoiler animation
  * `checklist` - Optional. Message is a checklist
  * `contact` - Optional. Message is a shared contact, information about the contact
  * `dice` - Optional. Message is a dice with random value
  * `game` - Optional. Message is a game, information about the game. More about games »
  * `poll` - Optional. Message is a native poll, information about the poll
  * `venue` - Optional. Message is a venue, information about the venue. For backward compatibility, when this field is set, the location field will also be set
  * `location` - Optional. Message is a shared location, information about the location
  * `new_chat_members` - Optional. New members that were added to the group or supergroup and information about them (the bot itself may be one of these members)
  * `left_chat_member` - Optional. A member was removed from the group, information about them (this member may be the bot itself)
  * `new_chat_title` - Optional. A chat title was changed to this value
  * `new_chat_photo` - Optional. A chat photo was change to this value
  * `delete_chat_photo` - Optional. Service message: the chat photo was deleted
  * `group_chat_created` - Optional. Service message: the group has been created
  * `supergroup_chat_created` - Optional. Service message: the supergroup has been created. This field can't be received in a message coming through updates, because bot can't be a member of a supergroup when it is created. It can only be found in reply_to_message if someone replies to a very first message in a directly created supergroup.
  * `channel_chat_created` - Optional. Service message: the channel has been created. This field can't be received in a message coming through updates, because bot can't be a member of a channel when it is created. It can only be found in reply_to_message if someone replies to a very first message in a channel.
  * `message_auto_delete_timer_changed` - Optional. Service message: auto-delete timer settings changed in the chat
  * `migrate_to_chat_id` - Optional. The group has been migrated to a supergroup with the specified identifier. This number may have more than 32 significant bits and some programming languages may have difficulty/silent defects in interpreting it. But it has at most 52 significant bits, so a signed 64-bit integer or double-precision float type are safe for storing this identifier.
  * `migrate_from_chat_id` - Optional. The supergroup has been migrated from a group with the specified identifier. This number may have more than 32 significant bits and some programming languages may have difficulty/silent defects in interpreting it. But it has at most 52 significant bits, so a signed 64-bit integer or double-precision float type are safe for storing this identifier.
  * `pinned_message` - Optional. Specified message was pinned. Note that the Message object in this field will not contain further reply_to_message fields even if it itself is a reply.
  * `invoice` - Optional. Message is an invoice for a payment, information about the invoice. More about payments »
  * `successful_payment` - Optional. Message is a service message about a successful payment, information about the payment. More about payments »
  * `refunded_payment` - Optional. Message is a service message about a refunded payment, information about the payment. More about payments »
  * `users_shared` - Optional. Service message: users were shared with the bot
  * `chat_shared` - Optional. Service message: a chat was shared with the bot
  * `gift` - Optional. Service message: a regular gift was sent or received
  * `unique_gift` - Optional. Service message: a unique gift was sent or received
  * `gift_upgrade_sent` - Optional. Service message: upgrade of a gift was purchased after the gift was sent
  * `connected_website` - Optional. The domain name of the website on which the user has logged in. More about Telegram Login »
  * `write_access_allowed` - Optional. Service message: the user allowed the bot to write messages after adding it to the attachment or side menu, launching a Web App from a link, or accepting an explicit request from a Web App sent by the method requestWriteAccess
  * `passport_data` - Optional. Telegram Passport data
  * `proximity_alert_triggered` - Optional. Service message. A user in the chat triggered another user's proximity alert while sharing Live Location.
  * `boost_added` - Optional. Service message: user boosted the chat
  * `chat_background_set` - Optional. Service message: chat background set
  * `checklist_tasks_done` - Optional. Service message: some tasks in a checklist were marked as done or not done
  * `checklist_tasks_added` - Optional. Service message: tasks were added to a checklist
  * `direct_message_price_changed` - Optional. Service message: the price for paid messages in the corresponding direct messages chat of a channel has changed
  * `forum_topic_created` - Optional. Service message: forum topic created
  * `forum_topic_edited` - Optional. Service message: forum topic edited
  * `forum_topic_closed` - Optional. Service message: forum topic closed
  * `forum_topic_reopened` - Optional. Service message: forum topic reopened
  * `general_forum_topic_hidden` - Optional. Service message: the 'General' forum topic hidden
  * `general_forum_topic_unhidden` - Optional. Service message: the 'General' forum topic unhidden
  * `giveaway_created` - Optional. Service message: a scheduled giveaway was created
  * `giveaway` - Optional. The message is a scheduled giveaway message
  * `giveaway_winners` - Optional. A giveaway with public winners was completed
  * `giveaway_completed` - Optional. Service message: a giveaway without public winners was completed
  * `paid_message_price_changed` - Optional. Service message: the price for paid messages has changed in the chat
  * `suggested_post_approved` - Optional. Service message: a suggested post was approved
  * `suggested_post_approval_failed` - Optional. Service message: approval of a suggested post has failed
  * `suggested_post_declined` - Optional. Service message: a suggested post was declined
  * `suggested_post_paid` - Optional. Service message: payment for a suggested post was received
  * `suggested_post_refunded` - Optional. Service message: payment for a suggested post was refunded
  * `video_chat_scheduled` - Optional. Service message: video chat scheduled
  * `video_chat_started` - Optional. Service message: video chat started
  * `video_chat_ended` - Optional. Service message: video chat ended
  * `video_chat_participants_invited` - Optional. Service message: new participants invited to a video chat
  * `web_app_data` - Optional. Service message: data sent by a Web App
  * `reply_markup` - Optional. Inline keyboard attached to the message. login_url buttons are represented as ordinary url buttons.
""".
-type 'Message'() :: #{
	message_id := integer(),
	message_thread_id => integer(),
	direct_messages_topic => 'DirectMessagesTopic'(),
	from => 'User'(),
	sender_chat => 'Chat'(),
	sender_boost_count => integer(),
	sender_business_bot => 'User'(),
	date := integer(),
	business_connection_id => binary(),
	chat := 'Chat'(),
	forward_origin => 'MessageOrigin'(),
	is_topic_message => true,
	is_automatic_forward => true,
	reply_to_message => 'Message'(),
	external_reply => 'ExternalReplyInfo'(),
	quote => 'TextQuote'(),
	reply_to_story => 'Story'(),
	reply_to_checklist_task_id => integer(),
	via_bot => 'User'(),
	edit_date => integer(),
	has_protected_content => true,
	is_from_offline => true,
	is_paid_post => true,
	media_group_id => binary(),
	author_signature => binary(),
	paid_star_count => integer(),
	text => binary(),
	entities => nonempty_list('MessageEntity'()),
	link_preview_options => 'LinkPreviewOptions'(),
	suggested_post_info => 'SuggestedPostInfo'(),
	effect_id => binary(),
	animation => 'Animation'(),
	audio => 'Audio'(),
	document => 'Document'(),
	paid_media => 'PaidMediaInfo'(),
	photo => nonempty_list('PhotoSize'()),
	sticker => 'Sticker'(),
	story => 'Story'(),
	video => 'Video'(),
	video_note => 'VideoNote'(),
	voice => 'Voice'(),
	caption => binary(),
	caption_entities => nonempty_list('MessageEntity'()),
	show_caption_above_media => true,
	has_media_spoiler => true,
	checklist => 'Checklist'(),
	contact => 'Contact'(),
	dice => 'Dice'(),
	game => 'Game'(),
	poll => 'Poll'(),
	venue => 'Venue'(),
	location => 'Location'(),
	new_chat_members => nonempty_list('User'()),
	left_chat_member => 'User'(),
	new_chat_title => binary(),
	new_chat_photo => nonempty_list('PhotoSize'()),
	delete_chat_photo => true,
	group_chat_created => true,
	supergroup_chat_created => true,
	channel_chat_created => true,
	message_auto_delete_timer_changed => 'MessageAutoDeleteTimerChanged'(),
	migrate_to_chat_id => integer(),
	migrate_from_chat_id => integer(),
	pinned_message => 'MaybeInaccessibleMessage'(),
	invoice => 'Invoice'(),
	successful_payment => 'SuccessfulPayment'(),
	refunded_payment => 'RefundedPayment'(),
	users_shared => 'UsersShared'(),
	chat_shared => 'ChatShared'(),
	gift => 'GiftInfo'(),
	unique_gift => 'UniqueGiftInfo'(),
	gift_upgrade_sent => 'GiftInfo'(),
	connected_website => binary(),
	write_access_allowed => 'WriteAccessAllowed'(),
	passport_data => 'PassportData'(),
	proximity_alert_triggered => 'ProximityAlertTriggered'(),
	boost_added => 'ChatBoostAdded'(),
	chat_background_set => 'ChatBackground'(),
	checklist_tasks_done => 'ChecklistTasksDone'(),
	checklist_tasks_added => 'ChecklistTasksAdded'(),
	direct_message_price_changed => 'DirectMessagePriceChanged'(),
	forum_topic_created => 'ForumTopicCreated'(),
	forum_topic_edited => 'ForumTopicEdited'(),
	forum_topic_closed => 'ForumTopicClosed'(),
	forum_topic_reopened => 'ForumTopicReopened'(),
	general_forum_topic_hidden => 'GeneralForumTopicHidden'(),
	general_forum_topic_unhidden => 'GeneralForumTopicUnhidden'(),
	giveaway_created => 'GiveawayCreated'(),
	giveaway => 'Giveaway'(),
	giveaway_winners => 'GiveawayWinners'(),
	giveaway_completed => 'GiveawayCompleted'(),
	paid_message_price_changed => 'PaidMessagePriceChanged'(),
	suggested_post_approved => 'SuggestedPostApproved'(),
	suggested_post_approval_failed => 'SuggestedPostApprovalFailed'(),
	suggested_post_declined => 'SuggestedPostDeclined'(),
	suggested_post_paid => 'SuggestedPostPaid'(),
	suggested_post_refunded => 'SuggestedPostRefunded'(),
	video_chat_scheduled => 'VideoChatScheduled'(),
	video_chat_started => 'VideoChatStarted'(),
	video_chat_ended => 'VideoChatEnded'(),
	video_chat_participants_invited => 'VideoChatParticipantsInvited'(),
	web_app_data => 'WebAppData'(),
	reply_markup => 'InlineKeyboardMarkup'()
}.

-doc """
This object represents a unique message identifier.
  * `message_id` - Unique message identifier. In specific instances (e.g., message containing a video sent to a big chat), the server might automatically schedule a message instead of sending it immediately. In such cases, this field will be 0 and the relevant message will be unusable until it is actually sent
""".
-type 'MessageId'() :: #{
	message_id := integer()
}.

-doc """
This object describes a message that was deleted or is otherwise inaccessible to the bot.
  * `chat` - Chat the message belonged to
  * `message_id` - Unique message identifier inside the chat
  * `date` - Always 0. The field can be used to differentiate regular and inaccessible messages.
""".
-type 'InaccessibleMessage'() :: #{
	chat := 'Chat'(),
	message_id := integer(),
	date := integer()
}.

-doc """
This object describes a message that can be inaccessible to the bot.  

""".
-type 'MaybeInaccessibleMessage'() :: 'Message'() | 'InaccessibleMessage'().

-doc """
This object represents one special entity in a text message.  
For example, hashtags, usernames, URLs, etc.
  * `type` - Type of the entity. Currently, can be “mention” (@username), “hashtag” (#hashtag or #hashtag@chatusername), “cashtag” ($USD or $USD@chatusername), “bot_command” (/start@jobs_bot), “url” (https://telegram.org), “email” (do-not-reply@telegram.org), “phone_number” (+1-212-555-0123), “bold” (bold text), “italic” (italic text), “underline” (underlined text), “strikethrough” (strikethrough text), “spoiler” (spoiler message), “blockquote” (block quotation), “expandable_blockquote” (collapsed-by-default block quotation), “code” (monowidth string), “pre” (monowidth block), “text_link” (for clickable text URLs), “text_mention” (for users without usernames), “custom_emoji” (for inline custom emoji stickers)
  * `offset` - Offset in UTF-16 code units to the start of the entity
  * `length` - Length of the entity in UTF-16 code units
  * `url` - Optional. For “text_link” only, URL that will be opened after user taps on the text
  * `user` - Optional. For “text_mention” only, the mentioned user
  * `language` - Optional. For “pre” only, the programming language of the entity text
  * `custom_emoji_id` - Optional. For “custom_emoji” only, unique identifier of the custom emoji. Use getCustomEmojiStickers to get full information about the sticker
""".
-type 'MessageEntity'() :: #{
	type := binary(),
	offset := integer(),
	length := integer(),
	url => binary(),
	user => 'User'(),
	language => binary(),
	custom_emoji_id => binary()
}.

-doc """
This object contains information about the quoted part of a message that is replied to by the given message.
  * `text` - Text of the quoted part of a message that is replied to by the given message
  * `entities` - Optional. Special entities that appear in the quote. Currently, only bold, italic, underline, strikethrough, spoiler, and custom_emoji entities are kept in quotes.
  * `position` - Approximate quote position in the original message in UTF-16 code units as specified by the sender
  * `is_manual` - Optional. True, if the quote was chosen manually by the message sender. Otherwise, the quote was added automatically by the server.
""".
-type 'TextQuote'() :: #{
	text := binary(),
	entities => nonempty_list('MessageEntity'()),
	position := integer(),
	is_manual => true
}.

-doc """
This object contains information about a message that is being replied to, which may come from another chat or forum topic.
  * `origin` - Origin of the message replied to by the given message
  * `chat` - Optional. Chat the original message belongs to. Available only if the chat is a supergroup or a channel.
  * `message_id` - Optional. Unique message identifier inside the original chat. Available only if the original chat is a supergroup or a channel.
  * `link_preview_options` - Optional. Options used for link preview generation for the original message, if it is a text message
  * `animation` - Optional. Message is an animation, information about the animation
  * `audio` - Optional. Message is an audio file, information about the file
  * `document` - Optional. Message is a general file, information about the file
  * `paid_media` - Optional. Message contains paid media; information about the paid media
  * `photo` - Optional. Message is a photo, available sizes of the photo
  * `sticker` - Optional. Message is a sticker, information about the sticker
  * `story` - Optional. Message is a forwarded story
  * `video` - Optional. Message is a video, information about the video
  * `video_note` - Optional. Message is a video note, information about the video message
  * `voice` - Optional. Message is a voice message, information about the file
  * `has_media_spoiler` - Optional. True, if the message media is covered by a spoiler animation
  * `checklist` - Optional. Message is a checklist
  * `contact` - Optional. Message is a shared contact, information about the contact
  * `dice` - Optional. Message is a dice with random value
  * `game` - Optional. Message is a game, information about the game. More about games »
  * `giveaway` - Optional. Message is a scheduled giveaway, information about the giveaway
  * `giveaway_winners` - Optional. A giveaway with public winners was completed
  * `invoice` - Optional. Message is an invoice for a payment, information about the invoice. More about payments »
  * `location` - Optional. Message is a shared location, information about the location
  * `poll` - Optional. Message is a native poll, information about the poll
  * `venue` - Optional. Message is a venue, information about the venue
""".
-type 'ExternalReplyInfo'() :: #{
	origin := 'MessageOrigin'(),
	chat => 'Chat'(),
	message_id => integer(),
	link_preview_options => 'LinkPreviewOptions'(),
	animation => 'Animation'(),
	audio => 'Audio'(),
	document => 'Document'(),
	paid_media => 'PaidMediaInfo'(),
	photo => nonempty_list('PhotoSize'()),
	sticker => 'Sticker'(),
	story => 'Story'(),
	video => 'Video'(),
	video_note => 'VideoNote'(),
	voice => 'Voice'(),
	has_media_spoiler => true,
	checklist => 'Checklist'(),
	contact => 'Contact'(),
	dice => 'Dice'(),
	game => 'Game'(),
	giveaway => 'Giveaway'(),
	giveaway_winners => 'GiveawayWinners'(),
	invoice => 'Invoice'(),
	location => 'Location'(),
	poll => 'Poll'(),
	venue => 'Venue'()
}.

-doc """
Describes reply parameters for the message that is being sent.
  * `message_id` - Identifier of the message that will be replied to in the current chat, or in the chat chat_id if it is specified
  * `chat_id` - Optional. If the message to be replied to is from a different chat, unique identifier for the chat or username of the channel (in the format @channelusername). Not supported for messages sent on behalf of a business account and messages from channel direct messages chats.
  * `allow_sending_without_reply` - Optional. Pass True if the message should be sent even if the specified message to be replied to is not found. Always False for replies in another chat or forum topic. Always True for messages sent on behalf of a business account.
  * `quote` - Optional. Quoted part of the message to be replied to; 0-1024 characters after entities parsing. The quote must be an exact substring of the message to be replied to, including bold, italic, underline, strikethrough, spoiler, and custom_emoji entities. The message will fail to send if the quote isn't found in the original message.
  * `quote_parse_mode` - Optional. Mode for parsing entities in the quote. See formatting options for more details.
  * `quote_entities` - Optional. A JSON-serialized list of special entities that appear in the quote. It can be specified instead of quote_parse_mode.
  * `quote_position` - Optional. Position of the quote in the original message in UTF-16 code units
  * `checklist_task_id` - Optional. Identifier of the specific checklist task to be replied to
""".
-type 'ReplyParameters'() :: #{
	message_id := integer(),
	chat_id => integer() | binary(),
	allow_sending_without_reply => boolean(),
	quote => binary(),
	quote_parse_mode => binary(),
	quote_entities => nonempty_list('MessageEntity'()),
	quote_position => integer(),
	checklist_task_id => integer()
}.

-doc """
This object describes the origin of a message.  

""".
-type 'MessageOrigin'() :: 'MessageOriginUser'() | 'MessageOriginHiddenUser'() | 'MessageOriginChat'() | 'MessageOriginChannel'().

-doc """
The message was originally sent by a known user.
  * `type` - Type of the message origin, always “user”
  * `date` - Date the message was sent originally in Unix time
  * `sender_user` - User that sent the message originally
""".
-type 'MessageOriginUser'() :: #{
	type := binary(),
	date := integer(),
	sender_user := 'User'()
}.

-doc """
The message was originally sent by an unknown user.
  * `type` - Type of the message origin, always “hidden_user”
  * `date` - Date the message was sent originally in Unix time
  * `sender_user_name` - Name of the user that sent the message originally
""".
-type 'MessageOriginHiddenUser'() :: #{
	type := binary(),
	date := integer(),
	sender_user_name := binary()
}.

-doc """
The message was originally sent on behalf of a chat to a group chat.
  * `type` - Type of the message origin, always “chat”
  * `date` - Date the message was sent originally in Unix time
  * `sender_chat` - Chat that sent the message originally
  * `author_signature` - Optional. For messages originally sent by an anonymous chat administrator, original message author signature
""".
-type 'MessageOriginChat'() :: #{
	type := binary(),
	date := integer(),
	sender_chat := 'Chat'(),
	author_signature => binary()
}.

-doc """
The message was originally sent to a channel chat.
  * `type` - Type of the message origin, always “channel”
  * `date` - Date the message was sent originally in Unix time
  * `chat` - Channel chat to which the message was originally sent
  * `message_id` - Unique message identifier inside the chat
  * `author_signature` - Optional. Signature of the original post author
""".
-type 'MessageOriginChannel'() :: #{
	type := binary(),
	date := integer(),
	chat := 'Chat'(),
	message_id := integer(),
	author_signature => binary()
}.

-doc """
This object represents one size of a photo or a file / sticker thumbnail.
  * `file_id` - Identifier for this file, which can be used to download or reuse the file
  * `file_unique_id` - Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  * `width` - Photo width
  * `height` - Photo height
  * `file_size` - Optional. File size in bytes
""".
-type 'PhotoSize'() :: #{
	file_id := binary(),
	file_unique_id := binary(),
	width := integer(),
	height := integer(),
	file_size => integer()
}.

-doc """
This object represents an animation file (GIF or H.264/MPEG-4 AVC video without sound).
  * `file_id` - Identifier for this file, which can be used to download or reuse the file
  * `file_unique_id` - Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  * `width` - Video width as defined by the sender
  * `height` - Video height as defined by the sender
  * `duration` - Duration of the video in seconds as defined by the sender
  * `thumbnail` - Optional. Animation thumbnail as defined by the sender
  * `file_name` - Optional. Original animation filename as defined by the sender
  * `mime_type` - Optional. MIME type of the file as defined by the sender
  * `file_size` - Optional. File size in bytes. It can be bigger than 2^31 and some programming languages may have difficulty/silent defects in interpreting it. But it has at most 52 significant bits, so a signed 64-bit integer or double-precision float type are safe for storing this value.
""".
-type 'Animation'() :: #{
	file_id := binary(),
	file_unique_id := binary(),
	width := integer(),
	height := integer(),
	duration := integer(),
	thumbnail => 'PhotoSize'(),
	file_name => binary(),
	mime_type => binary(),
	file_size => integer()
}.

-doc """
This object represents an audio file to be treated as music by the Telegram clients.
  * `file_id` - Identifier for this file, which can be used to download or reuse the file
  * `file_unique_id` - Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  * `duration` - Duration of the audio in seconds as defined by the sender
  * `performer` - Optional. Performer of the audio as defined by the sender or by audio tags
  * `title` - Optional. Title of the audio as defined by the sender or by audio tags
  * `file_name` - Optional. Original filename as defined by the sender
  * `mime_type` - Optional. MIME type of the file as defined by the sender
  * `file_size` - Optional. File size in bytes. It can be bigger than 2^31 and some programming languages may have difficulty/silent defects in interpreting it. But it has at most 52 significant bits, so a signed 64-bit integer or double-precision float type are safe for storing this value.
  * `thumbnail` - Optional. Thumbnail of the album cover to which the music file belongs
""".
-type 'Audio'() :: #{
	file_id := binary(),
	file_unique_id := binary(),
	duration := integer(),
	performer => binary(),
	title => binary(),
	file_name => binary(),
	mime_type => binary(),
	file_size => integer(),
	thumbnail => 'PhotoSize'()
}.

-doc """
This object represents a general file (as opposed to photos, voice messages and audio files).
  * `file_id` - Identifier for this file, which can be used to download or reuse the file
  * `file_unique_id` - Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  * `thumbnail` - Optional. Document thumbnail as defined by the sender
  * `file_name` - Optional. Original filename as defined by the sender
  * `mime_type` - Optional. MIME type of the file as defined by the sender
  * `file_size` - Optional. File size in bytes. It can be bigger than 2^31 and some programming languages may have difficulty/silent defects in interpreting it. But it has at most 52 significant bits, so a signed 64-bit integer or double-precision float type are safe for storing this value.
""".
-type 'Document'() :: #{
	file_id := binary(),
	file_unique_id := binary(),
	thumbnail => 'PhotoSize'(),
	file_name => binary(),
	mime_type => binary(),
	file_size => integer()
}.

-doc """
This object represents a story.
  * `chat` - Chat that posted the story
  * `id` - Unique identifier for the story in the chat
""".
-type 'Story'() :: #{
	chat := 'Chat'(),
	id := integer()
}.

-doc """
This object represents a video file.
  * `file_id` - Identifier for this file, which can be used to download or reuse the file
  * `file_unique_id` - Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  * `width` - Video width as defined by the sender
  * `height` - Video height as defined by the sender
  * `duration` - Duration of the video in seconds as defined by the sender
  * `thumbnail` - Optional. Video thumbnail
  * `cover` - Optional. Available sizes of the cover of the video in the message
  * `start_timestamp` - Optional. Timestamp in seconds from which the video will play in the message
  * `file_name` - Optional. Original filename as defined by the sender
  * `mime_type` - Optional. MIME type of the file as defined by the sender
  * `file_size` - Optional. File size in bytes. It can be bigger than 2^31 and some programming languages may have difficulty/silent defects in interpreting it. But it has at most 52 significant bits, so a signed 64-bit integer or double-precision float type are safe for storing this value.
""".
-type 'Video'() :: #{
	file_id := binary(),
	file_unique_id := binary(),
	width := integer(),
	height := integer(),
	duration := integer(),
	thumbnail => 'PhotoSize'(),
	cover => nonempty_list('PhotoSize'()),
	start_timestamp => integer(),
	file_name => binary(),
	mime_type => binary(),
	file_size => integer()
}.

-doc """
This object represents a video message (available in Telegram apps as of v.4.0).
  * `file_id` - Identifier for this file, which can be used to download or reuse the file
  * `file_unique_id` - Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  * `length` - Video width and height (diameter of the video message) as defined by the sender
  * `duration` - Duration of the video in seconds as defined by the sender
  * `thumbnail` - Optional. Video thumbnail
  * `file_size` - Optional. File size in bytes
""".
-type 'VideoNote'() :: #{
	file_id := binary(),
	file_unique_id := binary(),
	length := integer(),
	duration := integer(),
	thumbnail => 'PhotoSize'(),
	file_size => integer()
}.

-doc """
This object represents a voice note.
  * `file_id` - Identifier for this file, which can be used to download or reuse the file
  * `file_unique_id` - Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  * `duration` - Duration of the audio in seconds as defined by the sender
  * `mime_type` - Optional. MIME type of the file as defined by the sender
  * `file_size` - Optional. File size in bytes. It can be bigger than 2^31 and some programming languages may have difficulty/silent defects in interpreting it. But it has at most 52 significant bits, so a signed 64-bit integer or double-precision float type are safe for storing this value.
""".
-type 'Voice'() :: #{
	file_id := binary(),
	file_unique_id := binary(),
	duration := integer(),
	mime_type => binary(),
	file_size => integer()
}.

-doc """
Describes the paid media added to a message.
  * `star_count` - The number of Telegram Stars that must be paid to buy access to the media
  * `paid_media` - Information about the paid media
""".
-type 'PaidMediaInfo'() :: #{
	star_count := integer(),
	paid_media := nonempty_list('PaidMedia'())
}.

-doc """
This object describes paid media.  

""".
-type 'PaidMedia'() :: 'PaidMediaPreview'() | 'PaidMediaPhoto'() | 'PaidMediaVideo'().

-doc """
The paid media isn't available before the payment.
  * `type` - Type of the paid media, always “preview”
  * `width` - Optional. Media width as defined by the sender
  * `height` - Optional. Media height as defined by the sender
  * `duration` - Optional. Duration of the media in seconds as defined by the sender
""".
-type 'PaidMediaPreview'() :: #{
	type := binary(),
	width => integer(),
	height => integer(),
	duration => integer()
}.

-doc """
The paid media is a photo.
  * `type` - Type of the paid media, always “photo”
  * `photo` - The photo
""".
-type 'PaidMediaPhoto'() :: #{
	type := binary(),
	photo := nonempty_list('PhotoSize'())
}.

-doc """
The paid media is a video.
  * `type` - Type of the paid media, always “video”
  * `video` - The video
""".
-type 'PaidMediaVideo'() :: #{
	type := binary(),
	video := 'Video'()
}.

-doc """
This object represents a phone contact.
  * `phone_number` - Contact's phone number
  * `first_name` - Contact's first name
  * `last_name` - Optional. Contact's last name
  * `user_id` - Optional. Contact's user identifier in Telegram. This number may have more than 32 significant bits and some programming languages may have difficulty/silent defects in interpreting it. But it has at most 52 significant bits, so a 64-bit integer or double-precision float type are safe for storing this identifier.
  * `vcard` - Optional. Additional data about the contact in the form of a vCard
""".
-type 'Contact'() :: #{
	phone_number := binary(),
	first_name := binary(),
	last_name => binary(),
	user_id => integer(),
	vcard => binary()
}.

-doc """
This object represents an animated emoji that displays a random value.
  * `emoji` - Emoji on which the dice throw animation is based
  * `value` - Value of the dice, 1-6 for “”, “” and “” base emoji, 1-5 for “” and “” base emoji, 1-64 for “” base emoji
""".
-type 'Dice'() :: #{
	emoji := binary(),
	value := integer()
}.

-doc """
This object contains information about one answer option in a poll.
  * `text` - Option text, 1-100 characters
  * `text_entities` - Optional. Special entities that appear in the option text. Currently, only custom emoji entities are allowed in poll option texts
  * `voter_count` - Number of users that voted for this option
""".
-type 'PollOption'() :: #{
	text := binary(),
	text_entities => nonempty_list('MessageEntity'()),
	voter_count := integer()
}.

-doc """
This object contains information about one answer option in a poll to be sent.
  * `text` - Option text, 1-100 characters
  * `text_parse_mode` - Optional. Mode for parsing entities in the text. See formatting options for more details. Currently, only custom emoji entities are allowed
  * `text_entities` - Optional. A JSON-serialized list of special entities that appear in the poll option text. It can be specified instead of text_parse_mode
""".
-type 'InputPollOption'() :: #{
	text := binary(),
	text_parse_mode => binary(),
	text_entities => nonempty_list('MessageEntity'())
}.

-doc """
This object represents an answer of a user in a non-anonymous poll.
  * `poll_id` - Unique poll identifier
  * `voter_chat` - Optional. The chat that changed the answer to the poll, if the voter is anonymous
  * `user` - Optional. The user that changed the answer to the poll, if the voter isn't anonymous
  * `option_ids` - 0-based identifiers of chosen answer options. May be empty if the vote was retracted.
""".
-type 'PollAnswer'() :: #{
	poll_id := binary(),
	voter_chat => 'Chat'(),
	user => 'User'(),
	option_ids := nonempty_list(integer())
}.

-doc """
This object contains information about a poll.
  * `id` - Unique poll identifier
  * `question` - Poll question, 1-300 characters
  * `question_entities` - Optional. Special entities that appear in the question. Currently, only custom emoji entities are allowed in poll questions
  * `options` - List of poll options
  * `total_voter_count` - Total number of users that voted in the poll
  * `is_closed` - True, if the poll is closed
  * `is_anonymous` - True, if the poll is anonymous
  * `type` - Poll type, currently can be “regular” or “quiz”
  * `allows_multiple_answers` - True, if the poll allows multiple answers
  * `correct_option_id` - Optional. 0-based identifier of the correct answer option. Available only for polls in the quiz mode, which are closed, or was sent (not forwarded) by the bot or to the private chat with the bot.
  * `explanation` - Optional. Text that is shown when a user chooses an incorrect answer or taps on the lamp icon in a quiz-style poll, 0-200 characters
  * `explanation_entities` - Optional. Special entities like usernames, URLs, bot commands, etc. that appear in the explanation
  * `open_period` - Optional. Amount of time in seconds the poll will be active after creation
  * `close_date` - Optional. Point in time (Unix timestamp) when the poll will be automatically closed
""".
-type 'Poll'() :: #{
	id := binary(),
	question := binary(),
	question_entities => nonempty_list('MessageEntity'()),
	options := nonempty_list('PollOption'()),
	total_voter_count := integer(),
	is_closed := boolean(),
	is_anonymous := boolean(),
	type := binary(),
	allows_multiple_answers := boolean(),
	correct_option_id => integer(),
	explanation => binary(),
	explanation_entities => nonempty_list('MessageEntity'()),
	open_period => integer(),
	close_date => integer()
}.

-doc """
Describes a task in a checklist.
  * `id` - Unique identifier of the task
  * `text` - Text of the task
  * `text_entities` - Optional. Special entities that appear in the task text
  * `completed_by_user` - Optional. User that completed the task; omitted if the task wasn't completed by a user
  * `completed_by_chat` - Optional. Chat that completed the task; omitted if the task wasn't completed by a chat
  * `completion_date` - Optional. Point in time (Unix timestamp) when the task was completed; 0 if the task wasn't completed
""".
-type 'ChecklistTask'() :: #{
	id := integer(),
	text := binary(),
	text_entities => nonempty_list('MessageEntity'()),
	completed_by_user => 'User'(),
	completed_by_chat => 'Chat'(),
	completion_date => integer()
}.

-doc """
Describes a checklist.
  * `title` - Title of the checklist
  * `title_entities` - Optional. Special entities that appear in the checklist title
  * `tasks` - List of tasks in the checklist
  * `others_can_add_tasks` - Optional. True, if users other than the creator of the list can add tasks to the list
  * `others_can_mark_tasks_as_done` - Optional. True, if users other than the creator of the list can mark tasks as done or not done
""".
-type 'Checklist'() :: #{
	title := binary(),
	title_entities => nonempty_list('MessageEntity'()),
	tasks := nonempty_list('ChecklistTask'()),
	others_can_add_tasks => true,
	others_can_mark_tasks_as_done => true
}.

-doc """
Describes a task to add to a checklist.
  * `id` - Unique identifier of the task; must be positive and unique among all task identifiers currently present in the checklist
  * `text` - Text of the task; 1-100 characters after entities parsing
  * `parse_mode` - Optional. Mode for parsing entities in the text. See formatting options for more details.
  * `text_entities` - Optional. List of special entities that appear in the text, which can be specified instead of parse_mode. Currently, only bold, italic, underline, strikethrough, spoiler, and custom_emoji entities are allowed.
""".
-type 'InputChecklistTask'() :: #{
	id := integer(),
	text := binary(),
	parse_mode => binary(),
	text_entities => nonempty_list('MessageEntity'())
}.

-doc """
Describes a checklist to create.
  * `title` - Title of the checklist; 1-255 characters after entities parsing
  * `parse_mode` - Optional. Mode for parsing entities in the title. See formatting options for more details.
  * `title_entities` - Optional. List of special entities that appear in the title, which can be specified instead of parse_mode. Currently, only bold, italic, underline, strikethrough, spoiler, and custom_emoji entities are allowed.
  * `tasks` - List of 1-30 tasks in the checklist
  * `others_can_add_tasks` - Optional. Pass True if other users can add tasks to the checklist
  * `others_can_mark_tasks_as_done` - Optional. Pass True if other users can mark tasks as done or not done in the checklist
""".
-type 'InputChecklist'() :: #{
	title := binary(),
	parse_mode => binary(),
	title_entities => nonempty_list('MessageEntity'()),
	tasks := nonempty_list('InputChecklistTask'()),
	others_can_add_tasks => boolean(),
	others_can_mark_tasks_as_done => boolean()
}.

-doc """
Describes a service message about checklist tasks marked as done or not done.
  * `checklist_message` - Optional. Message containing the checklist whose tasks were marked as done or not done. Note that the Message object in this field will not contain the reply_to_message field even if it itself is a reply.
  * `marked_as_done_task_ids` - Optional. Identifiers of the tasks that were marked as done
  * `marked_as_not_done_task_ids` - Optional. Identifiers of the tasks that were marked as not done
""".
-type 'ChecklistTasksDone'() :: #{
	checklist_message => 'Message'(),
	marked_as_done_task_ids => nonempty_list(integer()),
	marked_as_not_done_task_ids => nonempty_list(integer())
}.

-doc """
Describes a service message about tasks added to a checklist.
  * `checklist_message` - Optional. Message containing the checklist to which the tasks were added. Note that the Message object in this field will not contain the reply_to_message field even if it itself is a reply.
  * `tasks` - List of tasks added to the checklist
""".
-type 'ChecklistTasksAdded'() :: #{
	checklist_message => 'Message'(),
	tasks := nonempty_list('ChecklistTask'())
}.

-doc """
This object represents a point on the map.
  * `latitude` - Latitude as defined by the sender
  * `longitude` - Longitude as defined by the sender
  * `horizontal_accuracy` - Optional. The radius of uncertainty for the location, measured in meters; 0-1500
  * `live_period` - Optional. Time relative to the message sending date, during which the location can be updated; in seconds. For active live locations only.
  * `heading` - Optional. The direction in which user is moving, in degrees; 1-360. For active live locations only.
  * `proximity_alert_radius` - Optional. The maximum distance for proximity alerts about approaching another chat member, in meters. For sent live locations only.
""".
-type 'Location'() :: #{
	latitude := float(),
	longitude := float(),
	horizontal_accuracy => float(),
	live_period => integer(),
	heading => integer(),
	proximity_alert_radius => integer()
}.

-doc """
This object represents a venue.
  * `location` - Venue location. Can't be a live location
  * `title` - Name of the venue
  * `address` - Address of the venue
  * `foursquare_id` - Optional. Foursquare identifier of the venue
  * `foursquare_type` - Optional. Foursquare type of the venue. (For example, “arts_entertainment/default”, “arts_entertainment/aquarium” or “food/icecream”.)
  * `google_place_id` - Optional. Google Places identifier of the venue
  * `google_place_type` - Optional. Google Places type of the venue. (See supported types.)
""".
-type 'Venue'() :: #{
	location := 'Location'(),
	title := binary(),
	address := binary(),
	foursquare_id => binary(),
	foursquare_type => binary(),
	google_place_id => binary(),
	google_place_type => binary()
}.

-doc """
Describes data sent from a Web App to the bot.
  * `data` - The data. Be aware that a bad client can send arbitrary data in this field.
  * `button_text` - Text of the web_app keyboard button from which the Web App was opened. Be aware that a bad client can send arbitrary data in this field.
""".
-type 'WebAppData'() :: #{
	data := binary(),
	button_text := binary()
}.

-doc """
This object represents the content of a service message, sent whenever a user in the chat triggers a proximity alert set by another user.
  * `traveler` - User that triggered the alert
  * `watcher` - User that set the alert
  * `distance` - The distance between the users
""".
-type 'ProximityAlertTriggered'() :: #{
	traveler := 'User'(),
	watcher := 'User'(),
	distance := integer()
}.

-doc """
This object represents a service message about a change in auto-delete timer settings.
  * `message_auto_delete_time` - New auto-delete time for messages in the chat; in seconds
""".
-type 'MessageAutoDeleteTimerChanged'() :: #{
	message_auto_delete_time := integer()
}.

-doc """
This object represents a service message about a user boosting a chat.
  * `boost_count` - Number of boosts added by the user
""".
-type 'ChatBoostAdded'() :: #{
	boost_count := integer()
}.

-doc """
This object describes the way a background is filled based on the selected colors.  

""".
-type 'BackgroundFill'() :: 'BackgroundFillSolid'() | 'BackgroundFillGradient'() | 'BackgroundFillFreeformGradient'().

-doc """
The background is filled using the selected color.
  * `type` - Type of the background fill, always “solid”
  * `color` - The color of the background fill in the RGB24 format
""".
-type 'BackgroundFillSolid'() :: #{
	type := binary(),
	color := integer()
}.

-doc """
The background is a gradient fill.
  * `type` - Type of the background fill, always “gradient”
  * `top_color` - Top color of the gradient in the RGB24 format
  * `bottom_color` - Bottom color of the gradient in the RGB24 format
  * `rotation_angle` - Clockwise rotation angle of the background fill in degrees; 0-359
""".
-type 'BackgroundFillGradient'() :: #{
	type := binary(),
	top_color := integer(),
	bottom_color := integer(),
	rotation_angle := integer()
}.

-doc """
The background is a freeform gradient that rotates after every message in the chat.
  * `type` - Type of the background fill, always “freeform_gradient”
  * `colors` - A list of the 3 or 4 base colors that are used to generate the freeform gradient in the RGB24 format
""".
-type 'BackgroundFillFreeformGradient'() :: #{
	type := binary(),
	colors := nonempty_list(integer())
}.

-doc """
This object describes the type of a background.  

""".
-type 'BackgroundType'() :: 'BackgroundTypeFill'() | 'BackgroundTypeWallpaper'() | 'BackgroundTypePattern'() | 'BackgroundTypeChatTheme'().

-doc """
The background is automatically filled based on the selected colors.
  * `type` - Type of the background, always “fill”
  * `fill` - The background fill
  * `dark_theme_dimming` - Dimming of the background in dark themes, as a percentage; 0-100
""".
-type 'BackgroundTypeFill'() :: #{
	type := binary(),
	fill := 'BackgroundFill'(),
	dark_theme_dimming := integer()
}.

-doc """
The background is a wallpaper in the JPEG format.
  * `type` - Type of the background, always “wallpaper”
  * `document` - Document with the wallpaper
  * `dark_theme_dimming` - Dimming of the background in dark themes, as a percentage; 0-100
  * `is_blurred` - Optional. True, if the wallpaper is downscaled to fit in a 450x450 square and then box-blurred with radius 12
  * `is_moving` - Optional. True, if the background moves slightly when the device is tilted
""".
-type 'BackgroundTypeWallpaper'() :: #{
	type := binary(),
	document := 'Document'(),
	dark_theme_dimming := integer(),
	is_blurred => true,
	is_moving => true
}.

-doc """
The background is a .PNG or .TGV (gzipped subset of SVG with MIME type “application/x-tgwallpattern”) pattern to be combined with the background fill chosen by the user.
  * `type` - Type of the background, always “pattern”
  * `document` - Document with the pattern
  * `fill` - The background fill that is combined with the pattern
  * `intensity` - Intensity of the pattern when it is shown above the filled background; 0-100
  * `is_inverted` - Optional. True, if the background fill must be applied only to the pattern itself. All other pixels are black in this case. For dark themes only
  * `is_moving` - Optional. True, if the background moves slightly when the device is tilted
""".
-type 'BackgroundTypePattern'() :: #{
	type := binary(),
	document := 'Document'(),
	fill := 'BackgroundFill'(),
	intensity := integer(),
	is_inverted => true,
	is_moving => true
}.

-doc """
The background is taken directly from a built-in chat theme.
  * `type` - Type of the background, always “chat_theme”
  * `theme_name` - Name of the chat theme, which is usually an emoji
""".
-type 'BackgroundTypeChatTheme'() :: #{
	type := binary(),
	theme_name := binary()
}.

-doc """
This object represents a chat background.
  * `type` - Type of the background
""".
-type 'ChatBackground'() :: #{
	type := 'BackgroundType'()
}.

-doc """
This object represents a service message about a new forum topic created in the chat.
  * `name` - Name of the topic
  * `icon_color` - Color of the topic icon in RGB format
  * `icon_custom_emoji_id` - Optional. Unique identifier of the custom emoji shown as the topic icon
  * `is_name_implicit` - Optional. True, if the name of the topic wasn't specified explicitly by its creator and likely needs to be changed by the bot
""".
-type 'ForumTopicCreated'() :: #{
	name := binary(),
	icon_color := integer(),
	icon_custom_emoji_id => binary(),
	is_name_implicit => true
}.

-doc """
This object represents a service message about a forum topic closed in the chat.  
Currently holds no information.
""".
-type 'ForumTopicClosed'() :: empty_map().

-doc """
This object represents a service message about an edited forum topic.
  * `name` - Optional. New name of the topic, if it was edited
  * `icon_custom_emoji_id` - Optional. New identifier of the custom emoji shown as the topic icon, if it was edited; an empty string if the icon was removed
""".
-type 'ForumTopicEdited'() :: #{
	name => binary(),
	icon_custom_emoji_id => binary()
}.

-doc """
This object represents a service message about a forum topic reopened in the chat.  
Currently holds no information.
""".
-type 'ForumTopicReopened'() :: empty_map().

-doc """
This object represents a service message about General forum topic hidden in the chat.  
Currently holds no information.
""".
-type 'GeneralForumTopicHidden'() :: empty_map().

-doc """
This object represents a service message about General forum topic unhidden in the chat.  
Currently holds no information.
""".
-type 'GeneralForumTopicUnhidden'() :: empty_map().

-doc """
This object contains information about a user that was shared with the bot using a KeyboardButtonRequestUsers button.
  * `user_id` - Identifier of the shared user. This number may have more than 32 significant bits and some programming languages may have difficulty/silent defects in interpreting it. But it has at most 52 significant bits, so 64-bit integers or double-precision float types are safe for storing these identifiers. The bot may not have access to the user and could be unable to use this identifier, unless the user is already known to the bot by some other means.
  * `first_name` - Optional. First name of the user, if the name was requested by the bot
  * `last_name` - Optional. Last name of the user, if the name was requested by the bot
  * `username` - Optional. Username of the user, if the username was requested by the bot
  * `photo` - Optional. Available sizes of the chat photo, if the photo was requested by the bot
""".
-type 'SharedUser'() :: #{
	user_id := integer(),
	first_name => binary(),
	last_name => binary(),
	username => binary(),
	photo => nonempty_list('PhotoSize'())
}.

-doc """
This object contains information about the users whose identifiers were shared with the bot using a KeyboardButtonRequestUsers button.
  * `request_id` - Identifier of the request
  * `users` - Information about users shared with the bot.
""".
-type 'UsersShared'() :: #{
	request_id := integer(),
	users := nonempty_list('SharedUser'())
}.

-doc """
This object contains information about a chat that was shared with the bot using a KeyboardButtonRequestChat button.
  * `request_id` - Identifier of the request
  * `chat_id` - Identifier of the shared chat. This number may have more than 32 significant bits and some programming languages may have difficulty/silent defects in interpreting it. But it has at most 52 significant bits, so a 64-bit integer or double-precision float type are safe for storing this identifier. The bot may not have access to the chat and could be unable to use this identifier, unless the chat is already known to the bot by some other means.
  * `title` - Optional. Title of the chat, if the title was requested by the bot.
  * `username` - Optional. Username of the chat, if the username was requested by the bot and available.
  * `photo` - Optional. Available sizes of the chat photo, if the photo was requested by the bot
""".
-type 'ChatShared'() :: #{
	request_id := integer(),
	chat_id := integer(),
	title => binary(),
	username => binary(),
	photo => nonempty_list('PhotoSize'())
}.

-doc """
This object represents a service message about a user allowing a bot to write messages after adding it to the attachment menu, launching a Web App from a link, or accepting an explicit request from a Web App sent by the method requestWriteAccess.
  * `from_request` - Optional. True, if the access was granted after the user accepted an explicit request from a Web App sent by the method requestWriteAccess
  * `web_app_name` - Optional. Name of the Web App, if the access was granted when the Web App was launched from a link
  * `from_attachment_menu` - Optional. True, if the access was granted when the bot was added to the attachment or side menu
""".
-type 'WriteAccessAllowed'() :: #{
	from_request => boolean(),
	web_app_name => binary(),
	from_attachment_menu => boolean()
}.

-doc """
This object represents a service message about a video chat scheduled in the chat.
  * `start_date` - Point in time (Unix timestamp) when the video chat is supposed to be started by a chat administrator
""".
-type 'VideoChatScheduled'() :: #{
	start_date := integer()
}.

-doc """
This object represents a service message about a video chat started in the chat.  
Currently holds no information.
""".
-type 'VideoChatStarted'() :: empty_map().

-doc """
This object represents a service message about a video chat ended in the chat.
  * `duration` - Video chat duration in seconds
""".
-type 'VideoChatEnded'() :: #{
	duration := integer()
}.

-doc """
This object represents a service message about new members invited to a video chat.
  * `users` - New members that were invited to the video chat
""".
-type 'VideoChatParticipantsInvited'() :: #{
	users := nonempty_list('User'())
}.

-doc """
Describes a service message about a change in the price of paid messages within a chat.
  * `paid_message_star_count` - The new number of Telegram Stars that must be paid by non-administrator users of the supergroup chat for each sent message
""".
-type 'PaidMessagePriceChanged'() :: #{
	paid_message_star_count := integer()
}.

-doc """
Describes a service message about a change in the price of direct messages sent to a channel chat.
  * `are_direct_messages_enabled` - True, if direct messages are enabled for the channel chat; false otherwise
  * `direct_message_star_count` - Optional. The new number of Telegram Stars that must be paid by users for each direct message sent to the channel. Does not apply to users who have been exempted by administrators. Defaults to 0.
""".
-type 'DirectMessagePriceChanged'() :: #{
	are_direct_messages_enabled := boolean(),
	direct_message_star_count => integer()
}.

-doc """
Describes a service message about the approval of a suggested post.
  * `suggested_post_message` - Optional. Message containing the suggested post. Note that the Message object in this field will not contain the reply_to_message field even if it itself is a reply.
  * `price` - Optional. Amount paid for the post
  * `send_date` - Date when the post will be published
""".
-type 'SuggestedPostApproved'() :: #{
	suggested_post_message => 'Message'(),
	price => 'SuggestedPostPrice'(),
	send_date := integer()
}.

-doc """
Describes a service message about the failed approval of a suggested post.  
Currently, only caused by insufficient user funds at the time of approval.
  * `suggested_post_message` - Optional. Message containing the suggested post whose approval has failed. Note that the Message object in this field will not contain the reply_to_message field even if it itself is a reply.
  * `price` - Expected price of the post
""".
-type 'SuggestedPostApprovalFailed'() :: #{
	suggested_post_message => 'Message'(),
	price := 'SuggestedPostPrice'()
}.

-doc """
Describes a service message about the rejection of a suggested post.
  * `suggested_post_message` - Optional. Message containing the suggested post. Note that the Message object in this field will not contain the reply_to_message field even if it itself is a reply.
  * `comment` - Optional. Comment with which the post was declined
""".
-type 'SuggestedPostDeclined'() :: #{
	suggested_post_message => 'Message'(),
	comment => binary()
}.

-doc """
Describes a service message about a successful payment for a suggested post.
  * `suggested_post_message` - Optional. Message containing the suggested post. Note that the Message object in this field will not contain the reply_to_message field even if it itself is a reply.
  * `currency` - Currency in which the payment was made. Currently, one of “XTR” for Telegram Stars or “TON” for toncoins
  * `amount` - Optional. The amount of the currency that was received by the channel in nanotoncoins; for payments in toncoins only
  * `star_amount` - Optional. The amount of Telegram Stars that was received by the channel; for payments in Telegram Stars only
""".
-type 'SuggestedPostPaid'() :: #{
	suggested_post_message => 'Message'(),
	currency := binary(),
	amount => integer(),
	star_amount => 'StarAmount'()
}.

-doc """
Describes a service message about a payment refund for a suggested post.
  * `suggested_post_message` - Optional. Message containing the suggested post. Note that the Message object in this field will not contain the reply_to_message field even if it itself is a reply.
  * `reason` - Reason for the refund. Currently, one of “post_deleted” if the post was deleted within 24 hours of being posted or removed from scheduled messages without being posted, or “payment_refunded” if the payer refunded their payment.
""".
-type 'SuggestedPostRefunded'() :: #{
	suggested_post_message => 'Message'(),
	reason := binary()
}.

-doc """
This object represents a service message about the creation of a scheduled giveaway.
  * `prize_star_count` - Optional. The number of Telegram Stars to be split between giveaway winners; for Telegram Star giveaways only
""".
-type 'GiveawayCreated'() :: #{
	prize_star_count => integer()
}.

-doc """
This object represents a message about a scheduled giveaway.
  * `chats` - The list of chats which the user must join to participate in the giveaway
  * `winners_selection_date` - Point in time (Unix timestamp) when winners of the giveaway will be selected
  * `winner_count` - The number of users which are supposed to be selected as winners of the giveaway
  * `only_new_members` - Optional. True, if only users who join the chats after the giveaway started should be eligible to win
  * `has_public_winners` - Optional. True, if the list of giveaway winners will be visible to everyone
  * `prize_description` - Optional. Description of additional giveaway prize
  * `country_codes` - Optional. A list of two-letter ISO 3166-1 alpha-2 country codes indicating the countries from which eligible users for the giveaway must come. If empty, then all users can participate in the giveaway. Users with a phone number that was bought on Fragment can always participate in giveaways.
  * `prize_star_count` - Optional. The number of Telegram Stars to be split between giveaway winners; for Telegram Star giveaways only
  * `premium_subscription_month_count` - Optional. The number of months the Telegram Premium subscription won from the giveaway will be active for; for Telegram Premium giveaways only
""".
-type 'Giveaway'() :: #{
	chats := nonempty_list('Chat'()),
	winners_selection_date := integer(),
	winner_count := integer(),
	only_new_members => true,
	has_public_winners => true,
	prize_description => binary(),
	country_codes => nonempty_list(binary()),
	prize_star_count => integer(),
	premium_subscription_month_count => integer()
}.

-doc """
This object represents a message about the completion of a giveaway with public winners.
  * `chat` - The chat that created the giveaway
  * `giveaway_message_id` - Identifier of the message with the giveaway in the chat
  * `winners_selection_date` - Point in time (Unix timestamp) when winners of the giveaway were selected
  * `winner_count` - Total number of winners in the giveaway
  * `winners` - List of up to 100 winners of the giveaway
  * `additional_chat_count` - Optional. The number of other chats the user had to join in order to be eligible for the giveaway
  * `prize_star_count` - Optional. The number of Telegram Stars that were split between giveaway winners; for Telegram Star giveaways only
  * `premium_subscription_month_count` - Optional. The number of months the Telegram Premium subscription won from the giveaway will be active for; for Telegram Premium giveaways only
  * `unclaimed_prize_count` - Optional. Number of undistributed prizes
  * `only_new_members` - Optional. True, if only users who had joined the chats after the giveaway started were eligible to win
  * `was_refunded` - Optional. True, if the giveaway was canceled because the payment for it was refunded
  * `prize_description` - Optional. Description of additional giveaway prize
""".
-type 'GiveawayWinners'() :: #{
	chat := 'Chat'(),
	giveaway_message_id := integer(),
	winners_selection_date := integer(),
	winner_count := integer(),
	winners := nonempty_list('User'()),
	additional_chat_count => integer(),
	prize_star_count => integer(),
	premium_subscription_month_count => integer(),
	unclaimed_prize_count => integer(),
	only_new_members => true,
	was_refunded => true,
	prize_description => binary()
}.

-doc """
This object represents a service message about the completion of a giveaway without public winners.
  * `winner_count` - Number of winners in the giveaway
  * `unclaimed_prize_count` - Optional. Number of undistributed prizes
  * `giveaway_message` - Optional. Message with the giveaway that was completed, if it wasn't deleted
  * `is_star_giveaway` - Optional. True, if the giveaway is a Telegram Star giveaway. Otherwise, currently, the giveaway is a Telegram Premium giveaway.
""".
-type 'GiveawayCompleted'() :: #{
	winner_count := integer(),
	unclaimed_prize_count => integer(),
	giveaway_message => 'Message'(),
	is_star_giveaway => true
}.

-doc """
Describes the options used for link preview generation.
  * `is_disabled` - Optional. True, if the link preview is disabled
  * `url` - Optional. URL to use for the link preview. If empty, then the first URL found in the message text will be used
  * `prefer_small_media` - Optional. True, if the media in the link preview is supposed to be shrunk; ignored if the URL isn't explicitly specified or media size change isn't supported for the preview
  * `prefer_large_media` - Optional. True, if the media in the link preview is supposed to be enlarged; ignored if the URL isn't explicitly specified or media size change isn't supported for the preview
  * `show_above_text` - Optional. True, if the link preview must be shown above the message text; otherwise, the link preview will be shown below the message text
""".
-type 'LinkPreviewOptions'() :: #{
	is_disabled => boolean(),
	url => binary(),
	prefer_small_media => boolean(),
	prefer_large_media => boolean(),
	show_above_text => boolean()
}.

-doc """
Describes the price of a suggested post.
  * `currency` - Currency in which the post will be paid. Currently, must be one of “XTR” for Telegram Stars or “TON” for toncoins
  * `amount` - The amount of the currency that will be paid for the post in the smallest units of the currency, i.e. Telegram Stars or nanotoncoins. Currently, price in Telegram Stars must be between 5 and 100000, and price in nanotoncoins must be between 10000000 and 10000000000000.
""".
-type 'SuggestedPostPrice'() :: #{
	currency := binary(),
	amount := integer()
}.

-doc """
Contains information about a suggested post.
  * `state` - State of the suggested post. Currently, it can be one of “pending”, “approved”, “declined”.
  * `price` - Optional. Proposed price of the post. If the field is omitted, then the post is unpaid.
  * `send_date` - Optional. Proposed send date of the post. If the field is omitted, then the post can be published at any time within 30 days at the sole discretion of the user or administrator who approves it.
""".
-type 'SuggestedPostInfo'() :: #{
	state := binary(),
	price => 'SuggestedPostPrice'(),
	send_date => integer()
}.

-doc """
Contains parameters of a post that is being suggested by the bot.
  * `price` - Optional. Proposed price for the post. If the field is omitted, then the post is unpaid.
  * `send_date` - Optional. Proposed send date of the post. If specified, then the date must be between 300 second and 2678400 seconds (30 days) in the future. If the field is omitted, then the post can be published at any time within 30 days at the sole discretion of the user who approves it.
""".
-type 'SuggestedPostParameters'() :: #{
	price => 'SuggestedPostPrice'(),
	send_date => integer()
}.

-doc """
Describes a topic of a direct messages chat.
  * `topic_id` - Unique identifier of the topic. This number may have more than 32 significant bits and some programming languages may have difficulty/silent defects in interpreting it. But it has at most 52 significant bits, so a 64-bit integer or double-precision float type are safe for storing this identifier.
  * `user` - Optional. Information about the user that created the topic. Currently, it is always present
""".
-type 'DirectMessagesTopic'() :: #{
	topic_id := integer(),
	user => 'User'()
}.

-doc """
This object represent a user's profile pictures.
  * `total_count` - Total number of profile pictures the target user has
  * `photos` - Requested profile pictures (in up to 4 sizes each)
""".
-type 'UserProfilePhotos'() :: #{
	total_count := integer(),
	photos := nonempty_list(nonempty_list('PhotoSize'()))
}.

-doc """
This object represents a file ready to be downloaded.  
The file can be downloaded via the link https://api.telegram.org/file/bot<token>/<file_path>.  
It is guaranteed that the link will be valid for at least 1 hour.  
When the link expires, a new one can be requested by calling getFile.  
The maximum file size to download is 20 MB
  * `file_id` - Identifier for this file, which can be used to download or reuse the file
  * `file_unique_id` - Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  * `file_size` - Optional. File size in bytes. It can be bigger than 2^31 and some programming languages may have difficulty/silent defects in interpreting it. But it has at most 52 significant bits, so a signed 64-bit integer or double-precision float type are safe for storing this value.
  * `file_path` - Optional. File path. Use https://api.telegram.org/file/bot<token>/<file_path> to get the file.
""".
-type 'File'() :: #{
	file_id := binary(),
	file_unique_id := binary(),
	file_size => integer(),
	file_path => binary()
}.

-doc """
Describes a Web App.
  * `url` - An HTTPS URL of a Web App to be opened with additional data as specified in Initializing Web Apps
""".
-type 'WebAppInfo'() :: #{
	url := binary()
}.

-doc """
This object represents a custom keyboard with reply options (see Introduction to bots for details and examples).  
Not supported in channels and for messages sent on behalf of a Telegram Business account.
  * `keyboard` - Array of button rows, each represented by an Array of KeyboardButton objects
  * `is_persistent` - Optional. Requests clients to always show the keyboard when the regular keyboard is hidden. Defaults to false, in which case the custom keyboard can be hidden and opened with a keyboard icon.
  * `resize_keyboard` - Optional. Requests clients to resize the keyboard vertically for optimal fit (e.g., make the keyboard smaller if there are just two rows of buttons). Defaults to false, in which case the custom keyboard is always of the same height as the app's standard keyboard.
  * `one_time_keyboard` - Optional. Requests clients to hide the keyboard as soon as it's been used. The keyboard will still be available, but clients will automatically display the usual letter-keyboard in the chat - the user can press a special button in the input field to see the custom keyboard again. Defaults to false.
  * `input_field_placeholder` - Optional. The placeholder to be shown in the input field when the keyboard is active; 1-64 characters
  * `selective` - Optional. Use this parameter if you want to show the keyboard to specific users only. Targets: 1) users that are @mentioned in the text of the Message object; 2) if the bot's message is a reply to a message in the same chat and forum topic, sender of the original message.

Example: A user requests to change the bot's language, bot replies to the request with a keyboard to select the new language. Other users in the group don't see the keyboard.
""".
-type 'ReplyKeyboardMarkup'() :: #{
	keyboard := nonempty_list(nonempty_list('KeyboardButton'())),
	is_persistent => boolean(),
	resize_keyboard => boolean(),
	one_time_keyboard => boolean(),
	input_field_placeholder => binary(),
	selective => boolean()
}.

-doc """
This object represents one button of the reply keyboard.  
At most one of the optional fields must be used to specify type of the button.  
For simple text buttons, String can be used instead of this object to specify the button text.  
Note: request_users and request_chat options will only work in Telegram versions released after 3 February, 2023.  
Older clients will display unsupported message.
  * `text` - Text of the button. If none of the optional fields are used, it will be sent as a message when the button is pressed
  * `request_users` - Optional. If specified, pressing the button will open a list of suitable users. Identifiers of selected users will be sent to the bot in a “users_shared” service message. Available in private chats only.
  * `request_chat` - Optional. If specified, pressing the button will open a list of suitable chats. Tapping on a chat will send its identifier to the bot in a “chat_shared” service message. Available in private chats only.
  * `request_contact` - Optional. If True, the user's phone number will be sent as a contact when the button is pressed. Available in private chats only.
  * `request_location` - Optional. If True, the user's current location will be sent when the button is pressed. Available in private chats only.
  * `request_poll` - Optional. If specified, the user will be asked to create a poll and send it to the bot when the button is pressed. Available in private chats only.
  * `web_app` - Optional. If specified, the described Web App will be launched when the button is pressed. The Web App will be able to send a “web_app_data” service message. Available in private chats only.
""".
-type 'KeyboardButton'() :: #{
	text := binary(),
	request_users => 'KeyboardButtonRequestUsers'(),
	request_chat => 'KeyboardButtonRequestChat'(),
	request_contact => boolean(),
	request_location => boolean(),
	request_poll => 'KeyboardButtonPollType'(),
	web_app => 'WebAppInfo'()
}.

-doc """
This object defines the criteria used to request suitable users.  
Information about the selected users will be shared with the bot when the corresponding button is pressed.  
More about requesting users »
  * `request_id` - Signed 32-bit identifier of the request that will be received back in the UsersShared object. Must be unique within the message
  * `user_is_bot` - Optional. Pass True to request bots, pass False to request regular users. If not specified, no additional restrictions are applied.
  * `user_is_premium` - Optional. Pass True to request premium users, pass False to request non-premium users. If not specified, no additional restrictions are applied.
  * `max_quantity` - Optional. The maximum number of users to be selected; 1-10. Defaults to 1.
  * `request_name` - Optional. Pass True to request the users' first and last names
  * `request_username` - Optional. Pass True to request the users' usernames
  * `request_photo` - Optional. Pass True to request the users' photos
""".
-type 'KeyboardButtonRequestUsers'() :: #{
	request_id := integer(),
	user_is_bot => boolean(),
	user_is_premium => boolean(),
	max_quantity => integer(),
	request_name => boolean(),
	request_username => boolean(),
	request_photo => boolean()
}.

-doc """
This object defines the criteria used to request a suitable chat.  
Information about the selected chat will be shared with the bot when the corresponding button is pressed.  
The bot will be granted requested rights in the chat if appropriate.  
More about requesting chats ».
  * `request_id` - Signed 32-bit identifier of the request, which will be received back in the ChatShared object. Must be unique within the message
  * `chat_is_channel` - Pass True to request a channel chat, pass False to request a group or a supergroup chat.
  * `chat_is_forum` - Optional. Pass True to request a forum supergroup, pass False to request a non-forum chat. If not specified, no additional restrictions are applied.
  * `chat_has_username` - Optional. Pass True to request a supergroup or a channel with a username, pass False to request a chat without a username. If not specified, no additional restrictions are applied.
  * `chat_is_created` - Optional. Pass True to request a chat owned by the user. Otherwise, no additional restrictions are applied.
  * `user_administrator_rights` - Optional. A JSON-serialized object listing the required administrator rights of the user in the chat. The rights must be a superset of bot_administrator_rights. If not specified, no additional restrictions are applied.
  * `bot_administrator_rights` - Optional. A JSON-serialized object listing the required administrator rights of the bot in the chat. The rights must be a subset of user_administrator_rights. If not specified, no additional restrictions are applied.
  * `bot_is_member` - Optional. Pass True to request a chat with the bot as a member. Otherwise, no additional restrictions are applied.
  * `request_title` - Optional. Pass True to request the chat's title
  * `request_username` - Optional. Pass True to request the chat's username
  * `request_photo` - Optional. Pass True to request the chat's photo
""".
-type 'KeyboardButtonRequestChat'() :: #{
	request_id := integer(),
	chat_is_channel := boolean(),
	chat_is_forum => boolean(),
	chat_has_username => boolean(),
	chat_is_created => boolean(),
	user_administrator_rights => 'ChatAdministratorRights'(),
	bot_administrator_rights => 'ChatAdministratorRights'(),
	bot_is_member => boolean(),
	request_title => boolean(),
	request_username => boolean(),
	request_photo => boolean()
}.

-doc """
This object represents type of a poll, which is allowed to be created and sent when the corresponding button is pressed.
  * `type` - Optional. If quiz is passed, the user will be allowed to create only polls in the quiz mode. If regular is passed, only regular polls will be allowed. Otherwise, the user will be allowed to create a poll of any type.
""".
-type 'KeyboardButtonPollType'() :: #{
	type => binary()
}.

-doc """
Upon receiving a message with this object, Telegram clients will remove the current custom keyboard and display the default letter-keyboard.  
By default, custom keyboards are displayed until a new keyboard is sent by a bot.  
An exception is made for one-time keyboards that are hidden immediately after the user presses a button (see ReplyKeyboardMarkup).  
Not supported in channels and for messages sent on behalf of a Telegram Business account.
  * `remove_keyboard` - Requests clients to remove the custom keyboard (user will not be able to summon this keyboard; if you want to hide the keyboard from sight but keep it accessible, use one_time_keyboard in ReplyKeyboardMarkup)
  * `selective` - Optional. Use this parameter if you want to remove the keyboard for specific users only. Targets: 1) users that are @mentioned in the text of the Message object; 2) if the bot's message is a reply to a message in the same chat and forum topic, sender of the original message.

Example: A user votes in a poll, bot returns confirmation message in reply to the vote and removes the keyboard for that user, while still showing the keyboard with poll options to users who haven't voted yet.
""".
-type 'ReplyKeyboardRemove'() :: #{
	remove_keyboard := true,
	selective => boolean()
}.

-doc """
This object represents an inline keyboard that appears right next to the message it belongs to.
  * `inline_keyboard` - Array of button rows, each represented by an Array of InlineKeyboardButton objects
""".
-type 'InlineKeyboardMarkup'() :: #{
	inline_keyboard := nonempty_list(nonempty_list('InlineKeyboardButton'()))
}.

-doc """
This object represents one button of an inline keyboard.  
Exactly one of the optional fields must be used to specify type of the button.
  * `text` - Label text on the button
  * `url` - Optional. HTTP or tg:// URL to be opened when the button is pressed. Links tg://user?id=<user_id> can be used to mention a user by their identifier without using a username, if this is allowed by their privacy settings.
  * `callback_data` - Optional. Data to be sent in a callback query to the bot when the button is pressed, 1-64 bytes
  * `web_app` - Optional. Description of the Web App that will be launched when the user presses the button. The Web App will be able to send an arbitrary message on behalf of the user using the method answerWebAppQuery. Available only in private chats between a user and the bot. Not supported for messages sent on behalf of a Telegram Business account.
  * `login_url` - Optional. An HTTPS URL used to automatically authorize the user. Can be used as a replacement for the Telegram Login Widget.
  * `switch_inline_query` - Optional. If set, pressing the button will prompt the user to select one of their chats, open that chat and insert the bot's username and the specified inline query in the input field. May be empty, in which case just the bot's username will be inserted. Not supported for messages sent in channel direct messages chats and on behalf of a Telegram Business account.
  * `switch_inline_query_current_chat` - Optional. If set, pressing the button will insert the bot's username and the specified inline query in the current chat's input field. May be empty, in which case only the bot's username will be inserted.

This offers a quick way for the user to open your bot in inline mode in the same chat - good for selecting something from multiple options. Not supported in channels and for messages sent in channel direct messages chats and on behalf of a Telegram Business account.
  * `switch_inline_query_chosen_chat` - Optional. If set, pressing the button will prompt the user to select one of their chats of the specified type, open that chat and insert the bot's username and the specified inline query in the input field. Not supported for messages sent in channel direct messages chats and on behalf of a Telegram Business account.
  * `copy_text` - Optional. Description of the button that copies the specified text to the clipboard.
  * `callback_game` - Optional. Description of the game that will be launched when the user presses the button.

NOTE: This type of button must always be the first button in the first row.
  * `pay` - Optional. Specify True, to send a Pay button. Substrings “” and “XTR” in the buttons's text will be replaced with a Telegram Star icon.

NOTE: This type of button must always be the first button in the first row and can only be used in invoice messages.
""".
-type 'InlineKeyboardButton'() :: #{
	text := binary(),
	url => binary(),
	callback_data => binary(),
	web_app => 'WebAppInfo'(),
	login_url => 'LoginUrl'(),
	switch_inline_query => binary(),
	switch_inline_query_current_chat => binary(),
	switch_inline_query_chosen_chat => 'SwitchInlineQueryChosenChat'(),
	copy_text => 'CopyTextButton'(),
	callback_game => 'CallbackGame'(),
	pay => boolean()
}.

-doc """
This object represents a parameter of the inline keyboard button used to automatically authorize a user.  
Serves as a great replacement for the Telegram Login Widget when the user is coming from Telegram.  
All the user needs to do is tap/click a button and confirm that they want to log in:  
Telegram apps support these buttons as of version 5.7.  
Sample bot: @discussbot
  * `url` - An HTTPS URL to be opened with user authorization data added to the query string when the button is pressed. If the user refuses to provide authorization data, the original URL without information about the user will be opened. The data added is the same as described in Receiving authorization data.

NOTE: You must always check the hash of the received data to verify the authentication and the integrity of the data as described in Checking authorization.
  * `forward_text` - Optional. New text of the button in forwarded messages.
  * `bot_username` - Optional. Username of a bot, which will be used for user authorization. See Setting up a bot for more details. If not specified, the current bot's username will be assumed. The url's domain must be the same as the domain linked with the bot. See Linking your domain to the bot for more details.
  * `request_write_access` - Optional. Pass True to request the permission for your bot to send messages to the user.
""".
-type 'LoginUrl'() :: #{
	url := binary(),
	forward_text => binary(),
	bot_username => binary(),
	request_write_access => boolean()
}.

-doc """
This object represents an inline button that switches the current user to inline mode in a chosen chat, with an optional default inline query.
  * `query` - Optional. The default inline query to be inserted in the input field. If left empty, only the bot's username will be inserted
  * `allow_user_chats` - Optional. True, if private chats with users can be chosen
  * `allow_bot_chats` - Optional. True, if private chats with bots can be chosen
  * `allow_group_chats` - Optional. True, if group and supergroup chats can be chosen
  * `allow_channel_chats` - Optional. True, if channel chats can be chosen
""".
-type 'SwitchInlineQueryChosenChat'() :: #{
	query => binary(),
	allow_user_chats => boolean(),
	allow_bot_chats => boolean(),
	allow_group_chats => boolean(),
	allow_channel_chats => boolean()
}.

-doc """
This object represents an inline keyboard button that copies specified text to the clipboard.
  * `text` - The text to be copied to the clipboard; 1-256 characters
""".
-type 'CopyTextButton'() :: #{
	text := binary()
}.

-doc """
This object represents an incoming callback query from a callback button in an inline keyboard.  
If the button that originated the query was attached to a message sent by the bot, the field message will be present.  
If the button was attached to a message sent via the bot (in inline mode), the field inline_message_id will be present.  
Exactly one of the fields data or game_short_name will be present.  
NOTE: After the user presses a callback button, Telegram clients will display a progress bar until you call answerCallbackQuery.  
It is, therefore, necessary to react by calling answerCallbackQuery even if no notification to the user is needed (e.g., without specifying any of the optional parameters).
  * `id` - Unique identifier for this query
  * `from` - Sender
  * `message` - Optional. Message sent by the bot with the callback button that originated the query
  * `inline_message_id` - Optional. Identifier of the message sent via the bot in inline mode, that originated the query.
  * `chat_instance` - Global identifier, uniquely corresponding to the chat to which the message with the callback button was sent. Useful for high scores in games.
  * `data` - Optional. Data associated with the callback button. Be aware that the message originated the query can contain no callback buttons with this data.
  * `game_short_name` - Optional. Short name of a Game to be returned, serves as the unique identifier for the game
""".
-type 'CallbackQuery'() :: #{
	id := binary(),
	from := 'User'(),
	message => 'MaybeInaccessibleMessage'(),
	inline_message_id => binary(),
	chat_instance := binary(),
	data => binary(),
	game_short_name => binary()
}.

-doc """
Upon receiving a message with this object, Telegram clients will display a reply interface to the user (act as if the user has selected the bot's message and tapped 'Reply').  
This can be extremely useful if you want to create user-friendly step-by-step interfaces without having to sacrifice privacy mode.  
Not supported in channels and for messages sent on behalf of a Telegram Business account.  
Example: A poll bot for groups runs in privacy mode (only receives commands, replies to its messages and mentions).  
There could be two ways to create a new poll:    Explain the user how to send a command with parameters (e.g.  
/newpoll question answer1 answer2).  
May be appealing for hardcore users but lacks modern day polish.  
Guide the user through a step-by-step process.  
'Please send me your question', 'Cool, now let's add the first answer option', 'Great.  
Keep adding answer options, then send /done when you're ready'.  
The last option is definitely more attractive.  
And if you use ForceReply in your bot's questions, it will receive the user's answers even if it only receives replies, commands and mentions - without any extra work for the user.
  * `force_reply` - Shows reply interface to the user, as if they manually selected the bot's message and tapped 'Reply'
  * `input_field_placeholder` - Optional. The placeholder to be shown in the input field when the reply is active; 1-64 characters
  * `selective` - Optional. Use this parameter if you want to force reply from specific users only. Targets: 1) users that are @mentioned in the text of the Message object; 2) if the bot's message is a reply to a message in the same chat and forum topic, sender of the original message.
""".
-type 'ForceReply'() :: #{
	force_reply := true,
	input_field_placeholder => binary(),
	selective => boolean()
}.

-doc """
This object represents a chat photo.
  * `small_file_id` - File identifier of small (160x160) chat photo. This file_id can be used only for photo download and only for as long as the photo is not changed.
  * `small_file_unique_id` - Unique file identifier of small (160x160) chat photo, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  * `big_file_id` - File identifier of big (640x640) chat photo. This file_id can be used only for photo download and only for as long as the photo is not changed.
  * `big_file_unique_id` - Unique file identifier of big (640x640) chat photo, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
""".
-type 'ChatPhoto'() :: #{
	small_file_id := binary(),
	small_file_unique_id := binary(),
	big_file_id := binary(),
	big_file_unique_id := binary()
}.

-doc """
Represents an invite link for a chat.
  * `invite_link` - The invite link. If the link was created by another chat administrator, then the second part of the link will be replaced with “…”.
  * `creator` - Creator of the link
  * `creates_join_request` - True, if users joining the chat via the link need to be approved by chat administrators
  * `is_primary` - True, if the link is primary
  * `is_revoked` - True, if the link is revoked
  * `name` - Optional. Invite link name
  * `expire_date` - Optional. Point in time (Unix timestamp) when the link will expire or has been expired
  * `member_limit` - Optional. The maximum number of users that can be members of the chat simultaneously after joining the chat via this invite link; 1-99999
  * `pending_join_request_count` - Optional. Number of pending join requests created using this link
  * `subscription_period` - Optional. The number of seconds the subscription will be active for before the next payment
  * `subscription_price` - Optional. The amount of Telegram Stars a user must pay initially and after each subsequent subscription period to be a member of the chat using the link
""".
-type 'ChatInviteLink'() :: #{
	invite_link := binary(),
	creator := 'User'(),
	creates_join_request := boolean(),
	is_primary := boolean(),
	is_revoked := boolean(),
	name => binary(),
	expire_date => integer(),
	member_limit => integer(),
	pending_join_request_count => integer(),
	subscription_period => integer(),
	subscription_price => integer()
}.

-doc """
Represents the rights of an administrator in a chat.
  * `is_anonymous` - True, if the user's presence in the chat is hidden
  * `can_manage_chat` - True, if the administrator can access the chat event log, get boost list, see hidden supergroup and channel members, report spam messages, ignore slow mode, and send messages to the chat without paying Telegram Stars. Implied by any other administrator privilege.
  * `can_delete_messages` - True, if the administrator can delete messages of other users
  * `can_manage_video_chats` - True, if the administrator can manage video chats
  * `can_restrict_members` - True, if the administrator can restrict, ban or unban chat members, or access supergroup statistics
  * `can_promote_members` - True, if the administrator can add new administrators with a subset of their own privileges or demote administrators that they have promoted, directly or indirectly (promoted by administrators that were appointed by the user)
  * `can_change_info` - True, if the user is allowed to change the chat title, photo and other settings
  * `can_invite_users` - True, if the user is allowed to invite new users to the chat
  * `can_post_stories` - True, if the administrator can post stories to the chat
  * `can_edit_stories` - True, if the administrator can edit stories posted by other users, post stories to the chat page, pin chat stories, and access the chat's story archive
  * `can_delete_stories` - True, if the administrator can delete stories posted by other users
  * `can_post_messages` - Optional. True, if the administrator can post messages in the channel, approve suggested posts, or access channel statistics; for channels only
  * `can_edit_messages` - Optional. True, if the administrator can edit messages of other users and can pin messages; for channels only
  * `can_pin_messages` - Optional. True, if the user is allowed to pin messages; for groups and supergroups only
  * `can_manage_topics` - Optional. True, if the user is allowed to create, rename, close, and reopen forum topics; for supergroups only
  * `can_manage_direct_messages` - Optional. True, if the administrator can manage direct messages of the channel and decline suggested posts; for channels only
""".
-type 'ChatAdministratorRights'() :: #{
	is_anonymous := boolean(),
	can_manage_chat := boolean(),
	can_delete_messages := boolean(),
	can_manage_video_chats := boolean(),
	can_restrict_members := boolean(),
	can_promote_members := boolean(),
	can_change_info := boolean(),
	can_invite_users := boolean(),
	can_post_stories := boolean(),
	can_edit_stories := boolean(),
	can_delete_stories := boolean(),
	can_post_messages => boolean(),
	can_edit_messages => boolean(),
	can_pin_messages => boolean(),
	can_manage_topics => boolean(),
	can_manage_direct_messages => boolean()
}.

-doc """
This object represents changes in the status of a chat member.
  * `chat` - Chat the user belongs to
  * `from` - Performer of the action, which resulted in the change
  * `date` - Date the change was done in Unix time
  * `old_chat_member` - Previous information about the chat member
  * `new_chat_member` - New information about the chat member
  * `invite_link` - Optional. Chat invite link, which was used by the user to join the chat; for joining by invite link events only.
  * `via_join_request` - Optional. True, if the user joined the chat after sending a direct join request without using an invite link and being approved by an administrator
  * `via_chat_folder_invite_link` - Optional. True, if the user joined the chat via a chat folder invite link
""".
-type 'ChatMemberUpdated'() :: #{
	chat := 'Chat'(),
	from := 'User'(),
	date := integer(),
	old_chat_member := 'ChatMember'(),
	new_chat_member := 'ChatMember'(),
	invite_link => 'ChatInviteLink'(),
	via_join_request => boolean(),
	via_chat_folder_invite_link => boolean()
}.

-doc """
This object contains information about one member of a chat.  
Currently, the following 6 types of chat members are supported:
""".
-type 'ChatMember'() :: 'ChatMemberOwner'() | 'ChatMemberAdministrator'() | 'ChatMemberMember'() | 'ChatMemberRestricted'() | 'ChatMemberLeft'() | 'ChatMemberBanned'().

-doc """
Represents a chat member that owns the chat and has all administrator privileges.
  * `status` - The member's status in the chat, always “creator”
  * `user` - Information about the user
  * `is_anonymous` - True, if the user's presence in the chat is hidden
  * `custom_title` - Optional. Custom title for this user
""".
-type 'ChatMemberOwner'() :: #{
	status := binary(),
	user := 'User'(),
	is_anonymous := boolean(),
	custom_title => binary()
}.

-doc """
Represents a chat member that has some additional privileges.
  * `status` - The member's status in the chat, always “administrator”
  * `user` - Information about the user
  * `can_be_edited` - True, if the bot is allowed to edit administrator privileges of that user
  * `is_anonymous` - True, if the user's presence in the chat is hidden
  * `can_manage_chat` - True, if the administrator can access the chat event log, get boost list, see hidden supergroup and channel members, report spam messages, ignore slow mode, and send messages to the chat without paying Telegram Stars. Implied by any other administrator privilege.
  * `can_delete_messages` - True, if the administrator can delete messages of other users
  * `can_manage_video_chats` - True, if the administrator can manage video chats
  * `can_restrict_members` - True, if the administrator can restrict, ban or unban chat members, or access supergroup statistics
  * `can_promote_members` - True, if the administrator can add new administrators with a subset of their own privileges or demote administrators that they have promoted, directly or indirectly (promoted by administrators that were appointed by the user)
  * `can_change_info` - True, if the user is allowed to change the chat title, photo and other settings
  * `can_invite_users` - True, if the user is allowed to invite new users to the chat
  * `can_post_stories` - True, if the administrator can post stories to the chat
  * `can_edit_stories` - True, if the administrator can edit stories posted by other users, post stories to the chat page, pin chat stories, and access the chat's story archive
  * `can_delete_stories` - True, if the administrator can delete stories posted by other users
  * `can_post_messages` - Optional. True, if the administrator can post messages in the channel, approve suggested posts, or access channel statistics; for channels only
  * `can_edit_messages` - Optional. True, if the administrator can edit messages of other users and can pin messages; for channels only
  * `can_pin_messages` - Optional. True, if the user is allowed to pin messages; for groups and supergroups only
  * `can_manage_topics` - Optional. True, if the user is allowed to create, rename, close, and reopen forum topics; for supergroups only
  * `can_manage_direct_messages` - Optional. True, if the administrator can manage direct messages of the channel and decline suggested posts; for channels only
  * `custom_title` - Optional. Custom title for this user
""".
-type 'ChatMemberAdministrator'() :: #{
	status := binary(),
	user := 'User'(),
	can_be_edited := boolean(),
	is_anonymous := boolean(),
	can_manage_chat := boolean(),
	can_delete_messages := boolean(),
	can_manage_video_chats := boolean(),
	can_restrict_members := boolean(),
	can_promote_members := boolean(),
	can_change_info := boolean(),
	can_invite_users := boolean(),
	can_post_stories := boolean(),
	can_edit_stories := boolean(),
	can_delete_stories := boolean(),
	can_post_messages => boolean(),
	can_edit_messages => boolean(),
	can_pin_messages => boolean(),
	can_manage_topics => boolean(),
	can_manage_direct_messages => boolean(),
	custom_title => binary()
}.

-doc """
Represents a chat member that has no additional privileges or restrictions.
  * `status` - The member's status in the chat, always “member”
  * `user` - Information about the user
  * `until_date` - Optional. Date when the user's subscription will expire; Unix time
""".
-type 'ChatMemberMember'() :: #{
	status := binary(),
	user := 'User'(),
	until_date => integer()
}.

-doc """
Represents a chat member that is under certain restrictions in the chat.  
Supergroups only.
  * `status` - The member's status in the chat, always “restricted”
  * `user` - Information about the user
  * `is_member` - True, if the user is a member of the chat at the moment of the request
  * `can_send_messages` - True, if the user is allowed to send text messages, contacts, giveaways, giveaway winners, invoices, locations and venues
  * `can_send_audios` - True, if the user is allowed to send audios
  * `can_send_documents` - True, if the user is allowed to send documents
  * `can_send_photos` - True, if the user is allowed to send photos
  * `can_send_videos` - True, if the user is allowed to send videos
  * `can_send_video_notes` - True, if the user is allowed to send video notes
  * `can_send_voice_notes` - True, if the user is allowed to send voice notes
  * `can_send_polls` - True, if the user is allowed to send polls and checklists
  * `can_send_other_messages` - True, if the user is allowed to send animations, games, stickers and use inline bots
  * `can_add_web_page_previews` - True, if the user is allowed to add web page previews to their messages
  * `can_change_info` - True, if the user is allowed to change the chat title, photo and other settings
  * `can_invite_users` - True, if the user is allowed to invite new users to the chat
  * `can_pin_messages` - True, if the user is allowed to pin messages
  * `can_manage_topics` - True, if the user is allowed to create forum topics
  * `until_date` - Date when restrictions will be lifted for this user; Unix time. If 0, then the user is restricted forever
""".
-type 'ChatMemberRestricted'() :: #{
	status := binary(),
	user := 'User'(),
	is_member := boolean(),
	can_send_messages := boolean(),
	can_send_audios := boolean(),
	can_send_documents := boolean(),
	can_send_photos := boolean(),
	can_send_videos := boolean(),
	can_send_video_notes := boolean(),
	can_send_voice_notes := boolean(),
	can_send_polls := boolean(),
	can_send_other_messages := boolean(),
	can_add_web_page_previews := boolean(),
	can_change_info := boolean(),
	can_invite_users := boolean(),
	can_pin_messages := boolean(),
	can_manage_topics := boolean(),
	until_date := integer()
}.

-doc """
Represents a chat member that isn't currently a member of the chat, but may join it themselves.
  * `status` - The member's status in the chat, always “left”
  * `user` - Information about the user
""".
-type 'ChatMemberLeft'() :: #{
	status := binary(),
	user := 'User'()
}.

-doc """
Represents a chat member that was banned in the chat and can't return to the chat or view chat messages.
  * `status` - The member's status in the chat, always “kicked”
  * `user` - Information about the user
  * `until_date` - Date when restrictions will be lifted for this user; Unix time. If 0, then the user is banned forever
""".
-type 'ChatMemberBanned'() :: #{
	status := binary(),
	user := 'User'(),
	until_date := integer()
}.

-doc """
Represents a join request sent to a chat.
  * `chat` - Chat to which the request was sent
  * `from` - User that sent the join request
  * `user_chat_id` - Identifier of a private chat with the user who sent the join request. This number may have more than 32 significant bits and some programming languages may have difficulty/silent defects in interpreting it. But it has at most 52 significant bits, so a 64-bit integer or double-precision float type are safe for storing this identifier. The bot can use this identifier for 5 minutes to send messages until the join request is processed, assuming no other administrator contacted the user.
  * `date` - Date the request was sent in Unix time
  * `bio` - Optional. Bio of the user.
  * `invite_link` - Optional. Chat invite link that was used by the user to send the join request
""".
-type 'ChatJoinRequest'() :: #{
	chat := 'Chat'(),
	from := 'User'(),
	user_chat_id := integer(),
	date := integer(),
	bio => binary(),
	invite_link => 'ChatInviteLink'()
}.

-doc """
Describes actions that a non-administrator user is allowed to take in a chat.
  * `can_send_messages` - Optional. True, if the user is allowed to send text messages, contacts, giveaways, giveaway winners, invoices, locations and venues
  * `can_send_audios` - Optional. True, if the user is allowed to send audios
  * `can_send_documents` - Optional. True, if the user is allowed to send documents
  * `can_send_photos` - Optional. True, if the user is allowed to send photos
  * `can_send_videos` - Optional. True, if the user is allowed to send videos
  * `can_send_video_notes` - Optional. True, if the user is allowed to send video notes
  * `can_send_voice_notes` - Optional. True, if the user is allowed to send voice notes
  * `can_send_polls` - Optional. True, if the user is allowed to send polls and checklists
  * `can_send_other_messages` - Optional. True, if the user is allowed to send animations, games, stickers and use inline bots
  * `can_add_web_page_previews` - Optional. True, if the user is allowed to add web page previews to their messages
  * `can_change_info` - Optional. True, if the user is allowed to change the chat title, photo and other settings. Ignored in public supergroups
  * `can_invite_users` - Optional. True, if the user is allowed to invite new users to the chat
  * `can_pin_messages` - Optional. True, if the user is allowed to pin messages. Ignored in public supergroups
  * `can_manage_topics` - Optional. True, if the user is allowed to create forum topics. If omitted defaults to the value of can_pin_messages
""".
-type 'ChatPermissions'() :: #{
	can_send_messages => boolean(),
	can_send_audios => boolean(),
	can_send_documents => boolean(),
	can_send_photos => boolean(),
	can_send_videos => boolean(),
	can_send_video_notes => boolean(),
	can_send_voice_notes => boolean(),
	can_send_polls => boolean(),
	can_send_other_messages => boolean(),
	can_add_web_page_previews => boolean(),
	can_change_info => boolean(),
	can_invite_users => boolean(),
	can_pin_messages => boolean(),
	can_manage_topics => boolean()
}.

-doc """
Describes the birthdate of a user.
  * `day` - Day of the user's birth; 1-31
  * `month` - Month of the user's birth; 1-12
  * `year` - Optional. Year of the user's birth
""".
-type 'Birthdate'() :: #{
	day := integer(),
	month := integer(),
	year => integer()
}.

-doc """
Contains information about the start page settings of a Telegram Business account.
  * `title` - Optional. Title text of the business intro
  * `message` - Optional. Message text of the business intro
  * `sticker` - Optional. Sticker of the business intro
""".
-type 'BusinessIntro'() :: #{
	title => binary(),
	message => binary(),
	sticker => 'Sticker'()
}.

-doc """
Contains information about the location of a Telegram Business account.
  * `address` - Address of the business
  * `location` - Optional. Location of the business
""".
-type 'BusinessLocation'() :: #{
	address := binary(),
	location => 'Location'()
}.

-doc """
Describes an interval of time during which a business is open.
  * `opening_minute` - The minute's sequence number in a week, starting on Monday, marking the start of the time interval during which the business is open; 0 - 7 \* 24 \* 60
  * `closing_minute` - The minute's sequence number in a week, starting on Monday, marking the end of the time interval during which the business is open; 0 - 8 \* 24 \* 60
""".
-type 'BusinessOpeningHoursInterval'() :: #{
	opening_minute := integer(),
	closing_minute := integer()
}.

-doc """
Describes the opening hours of a business.
  * `time_zone_name` - Unique name of the time zone for which the opening hours are defined
  * `opening_hours` - List of time intervals describing business opening hours
""".
-type 'BusinessOpeningHours'() :: #{
	time_zone_name := binary(),
	opening_hours := nonempty_list('BusinessOpeningHoursInterval'())
}.

-doc """
This object describes the rating of a user based on their Telegram Star spendings.
  * `level` - Current level of the user, indicating their reliability when purchasing digital goods and services. A higher level suggests a more trustworthy customer; a negative level is likely reason for concern.
  * `rating` - Numerical value of the user's rating; the higher the rating, the better
  * `current_level_rating` - The rating value required to get the current level
  * `next_level_rating` - Optional. The rating value required to get to the next level; omitted if the maximum level was reached
""".
-type 'UserRating'() :: #{
	level := integer(),
	rating := integer(),
	current_level_rating := integer(),
	next_level_rating => integer()
}.

-doc """
Describes the position of a clickable area within a story.
  * `x_percentage` - The abscissa of the area's center, as a percentage of the media width
  * `y_percentage` - The ordinate of the area's center, as a percentage of the media height
  * `width_percentage` - The width of the area's rectangle, as a percentage of the media width
  * `height_percentage` - The height of the area's rectangle, as a percentage of the media height
  * `rotation_angle` - The clockwise rotation angle of the rectangle, in degrees; 0-360
  * `corner_radius_percentage` - The radius of the rectangle corner rounding, as a percentage of the media width
""".
-type 'StoryAreaPosition'() :: #{
	x_percentage := float(),
	y_percentage := float(),
	width_percentage := float(),
	height_percentage := float(),
	rotation_angle := float(),
	corner_radius_percentage := float()
}.

-doc """
Describes the physical address of a location.
  * `country_code` - The two-letter ISO 3166-1 alpha-2 country code of the country where the location is located
  * `state` - Optional. State of the location
  * `city` - Optional. City of the location
  * `street` - Optional. Street address of the location
""".
-type 'LocationAddress'() :: #{
	country_code := binary(),
	state => binary(),
	city => binary(),
	street => binary()
}.

-doc """
Describes the type of a clickable area on a story.  

""".
-type 'StoryAreaType'() :: 'StoryAreaTypeLocation'() | 'StoryAreaTypeSuggestedReaction'() | 'StoryAreaTypeLink'() | 'StoryAreaTypeWeather'() | 'StoryAreaTypeUniqueGift'().

-doc """
Describes a story area pointing to a location.  
Currently, a story can have up to 10 location areas.
  * `type` - Type of the area, always “location”
  * `latitude` - Location latitude in degrees
  * `longitude` - Location longitude in degrees
  * `address` - Optional. Address of the location
""".
-type 'StoryAreaTypeLocation'() :: #{
	type := binary(),
	latitude := float(),
	longitude := float(),
	address => 'LocationAddress'()
}.

-doc """
Describes a story area pointing to a suggested reaction.  
Currently, a story can have up to 5 suggested reaction areas.
  * `type` - Type of the area, always “suggested_reaction”
  * `reaction_type` - Type of the reaction
  * `is_dark` - Optional. Pass True if the reaction area has a dark background
  * `is_flipped` - Optional. Pass True if reaction area corner is flipped
""".
-type 'StoryAreaTypeSuggestedReaction'() :: #{
	type := binary(),
	reaction_type := 'ReactionType'(),
	is_dark => boolean(),
	is_flipped => boolean()
}.

-doc """
Describes a story area pointing to an HTTP or tg:// link.  
Currently, a story can have up to 3 link areas.
  * `type` - Type of the area, always “link”
  * `url` - HTTP or tg:// URL to be opened when the area is clicked
""".
-type 'StoryAreaTypeLink'() :: #{
	type := binary(),
	url := binary()
}.

-doc """
Describes a story area containing weather information.  
Currently, a story can have up to 3 weather areas.
  * `type` - Type of the area, always “weather”
  * `temperature` - Temperature, in degree Celsius
  * `emoji` - Emoji representing the weather
  * `background_color` - A color of the area background in the ARGB format
""".
-type 'StoryAreaTypeWeather'() :: #{
	type := binary(),
	temperature := float(),
	emoji := binary(),
	background_color := integer()
}.

-doc """
Describes a story area pointing to a unique gift.  
Currently, a story can have at most 1 unique gift area.
  * `type` - Type of the area, always “unique_gift”
  * `name` - Unique name of the gift
""".
-type 'StoryAreaTypeUniqueGift'() :: #{
	type := binary(),
	name := binary()
}.

-doc """
Describes a clickable area on a story media.
  * `position` - Position of the area
  * `type` - Type of the area
""".
-type 'StoryArea'() :: #{
	position := 'StoryAreaPosition'(),
	type := 'StoryAreaType'()
}.

-doc """
Represents a location to which a chat is connected.
  * `location` - The location to which the supergroup is connected. Can't be a live location.
  * `address` - Location address; 1-64 characters, as defined by the chat owner
""".
-type 'ChatLocation'() :: #{
	location := 'Location'(),
	address := binary()
}.

-doc """
This object describes the type of a reaction.  

""".
-type 'ReactionType'() :: 'ReactionTypeEmoji'() | 'ReactionTypeCustomEmoji'() | 'ReactionTypePaid'().

-doc """
The reaction is based on an emoji.
  * `type` - Type of the reaction, always “emoji”
  * `emoji` - Reaction emoji. Currently, it can be one of , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , ,
""".
-type 'ReactionTypeEmoji'() :: #{
	type := binary(),
	emoji := binary()
}.

-doc """
The reaction is based on a custom emoji.
  * `type` - Type of the reaction, always “custom_emoji”
  * `custom_emoji_id` - Custom emoji identifier
""".
-type 'ReactionTypeCustomEmoji'() :: #{
	type := binary(),
	custom_emoji_id := binary()
}.

-doc """
The reaction is paid.
  * `type` - Type of the reaction, always “paid”
""".
-type 'ReactionTypePaid'() :: #{
	type := binary()
}.

-doc """
Represents a reaction added to a message along with the number of times it was added.
  * `type` - Type of the reaction
  * `total_count` - Number of times the reaction was added
""".
-type 'ReactionCount'() :: #{
	type := 'ReactionType'(),
	total_count := integer()
}.

-doc """
This object represents a change of a reaction on a message performed by a user.
  * `chat` - The chat containing the message the user reacted to
  * `message_id` - Unique identifier of the message inside the chat
  * `user` - Optional. The user that changed the reaction, if the user isn't anonymous
  * `actor_chat` - Optional. The chat on behalf of which the reaction was changed, if the user is anonymous
  * `date` - Date of the change in Unix time
  * `old_reaction` - Previous list of reaction types that were set by the user
  * `new_reaction` - New list of reaction types that have been set by the user
""".
-type 'MessageReactionUpdated'() :: #{
	chat := 'Chat'(),
	message_id := integer(),
	user => 'User'(),
	actor_chat => 'Chat'(),
	date := integer(),
	old_reaction := nonempty_list('ReactionType'()),
	new_reaction := nonempty_list('ReactionType'())
}.

-doc """
This object represents reaction changes on a message with anonymous reactions.
  * `chat` - The chat containing the message
  * `message_id` - Unique message identifier inside the chat
  * `date` - Date of the change in Unix time
  * `reactions` - List of reactions that are present on the message
""".
-type 'MessageReactionCountUpdated'() :: #{
	chat := 'Chat'(),
	message_id := integer(),
	date := integer(),
	reactions := nonempty_list('ReactionCount'())
}.

-doc """
This object represents a forum topic.
  * `message_thread_id` - Unique identifier of the forum topic
  * `name` - Name of the topic
  * `icon_color` - Color of the topic icon in RGB format
  * `icon_custom_emoji_id` - Optional. Unique identifier of the custom emoji shown as the topic icon
  * `is_name_implicit` - Optional. True, if the name of the topic wasn't specified explicitly by its creator and likely needs to be changed by the bot
""".
-type 'ForumTopic'() :: #{
	message_thread_id := integer(),
	name := binary(),
	icon_color := integer(),
	icon_custom_emoji_id => binary(),
	is_name_implicit => true
}.

-doc """
This object describes the background of a gift.
  * `center_color` - Center color of the background in RGB format
  * `edge_color` - Edge color of the background in RGB format
  * `text_color` - Text color of the background in RGB format
""".
-type 'GiftBackground'() :: #{
	center_color := integer(),
	edge_color := integer(),
	text_color := integer()
}.

-doc """
This object represents a gift that can be sent by the bot.
  * `id` - Unique identifier of the gift
  * `sticker` - The sticker that represents the gift
  * `star_count` - The number of Telegram Stars that must be paid to send the sticker
  * `upgrade_star_count` - Optional. The number of Telegram Stars that must be paid to upgrade the gift to a unique one
  * `is_premium` - Optional. True, if the gift can only be purchased by Telegram Premium subscribers
  * `has_colors` - Optional. True, if the gift can be used (after being upgraded) to customize a user's appearance
  * `total_count` - Optional. The total number of gifts of this type that can be sent by all users; for limited gifts only
  * `remaining_count` - Optional. The number of remaining gifts of this type that can be sent by all users; for limited gifts only
  * `personal_total_count` - Optional. The total number of gifts of this type that can be sent by the bot; for limited gifts only
  * `personal_remaining_count` - Optional. The number of remaining gifts of this type that can be sent by the bot; for limited gifts only
  * `background` - Optional. Background of the gift
  * `unique_gift_variant_count` - Optional. The total number of different unique gifts that can be obtained by upgrading the gift
  * `publisher_chat` - Optional. Information about the chat that published the gift
""".
-type 'Gift'() :: #{
	id := binary(),
	sticker := 'Sticker'(),
	star_count := integer(),
	upgrade_star_count => integer(),
	is_premium => true,
	has_colors => true,
	total_count => integer(),
	remaining_count => integer(),
	personal_total_count => integer(),
	personal_remaining_count => integer(),
	background => 'GiftBackground'(),
	unique_gift_variant_count => integer(),
	publisher_chat => 'Chat'()
}.

-doc """
This object represent a list of gifts.
  * `gifts` - The list of gifts
""".
-type 'Gifts'() :: #{
	gifts := nonempty_list('Gift'())
}.

-doc """
This object describes the model of a unique gift.
  * `name` - Name of the model
  * `sticker` - The sticker that represents the unique gift
  * `rarity_per_mille` - The number of unique gifts that receive this model for every 1000 gifts upgraded
""".
-type 'UniqueGiftModel'() :: #{
	name := binary(),
	sticker := 'Sticker'(),
	rarity_per_mille := integer()
}.

-doc """
This object describes the symbol shown on the pattern of a unique gift.
  * `name` - Name of the symbol
  * `sticker` - The sticker that represents the unique gift
  * `rarity_per_mille` - The number of unique gifts that receive this model for every 1000 gifts upgraded
""".
-type 'UniqueGiftSymbol'() :: #{
	name := binary(),
	sticker := 'Sticker'(),
	rarity_per_mille := integer()
}.

-doc """
This object describes the colors of the backdrop of a unique gift.
  * `center_color` - The color in the center of the backdrop in RGB format
  * `edge_color` - The color on the edges of the backdrop in RGB format
  * `symbol_color` - The color to be applied to the symbol in RGB format
  * `text_color` - The color for the text on the backdrop in RGB format
""".
-type 'UniqueGiftBackdropColors'() :: #{
	center_color := integer(),
	edge_color := integer(),
	symbol_color := integer(),
	text_color := integer()
}.

-doc """
This object describes the backdrop of a unique gift.
  * `name` - Name of the backdrop
  * `colors` - Colors of the backdrop
  * `rarity_per_mille` - The number of unique gifts that receive this backdrop for every 1000 gifts upgraded
""".
-type 'UniqueGiftBackdrop'() :: #{
	name := binary(),
	colors := 'UniqueGiftBackdropColors'(),
	rarity_per_mille := integer()
}.

-doc """
This object contains information about the color scheme for a user's name, message replies and link previews based on a unique gift.
  * `model_custom_emoji_id` - Custom emoji identifier of the unique gift's model
  * `symbol_custom_emoji_id` - Custom emoji identifier of the unique gift's symbol
  * `light_theme_main_color` - Main color used in light themes; RGB format
  * `light_theme_other_colors` - List of 1-3 additional colors used in light themes; RGB format
  * `dark_theme_main_color` - Main color used in dark themes; RGB format
  * `dark_theme_other_colors` - List of 1-3 additional colors used in dark themes; RGB format
""".
-type 'UniqueGiftColors'() :: #{
	model_custom_emoji_id := binary(),
	symbol_custom_emoji_id := binary(),
	light_theme_main_color := integer(),
	light_theme_other_colors := nonempty_list(integer()),
	dark_theme_main_color := integer(),
	dark_theme_other_colors := nonempty_list(integer())
}.

-doc """
This object describes a unique gift that was upgraded from a regular gift.
  * `gift_id` - Identifier of the regular gift from which the gift was upgraded
  * `base_name` - Human-readable name of the regular gift from which this unique gift was upgraded
  * `name` - Unique name of the gift. This name can be used in https://t.me/nft/... links and story areas
  * `number` - Unique number of the upgraded gift among gifts upgraded from the same regular gift
  * `model` - Model of the gift
  * `symbol` - Symbol of the gift
  * `backdrop` - Backdrop of the gift
  * `is_premium` - Optional. True, if the original regular gift was exclusively purchaseable by Telegram Premium subscribers
  * `is_from_blockchain` - Optional. True, if the gift is assigned from the TON blockchain and can't be resold or transferred in Telegram
  * `colors` - Optional. The color scheme that can be used by the gift's owner for the chat's name, replies to messages and link previews; for business account gifts and gifts that are currently on sale only
  * `publisher_chat` - Optional. Information about the chat that published the gift
""".
-type 'UniqueGift'() :: #{
	gift_id := binary(),
	base_name := binary(),
	name := binary(),
	number := integer(),
	model := 'UniqueGiftModel'(),
	symbol := 'UniqueGiftSymbol'(),
	backdrop := 'UniqueGiftBackdrop'(),
	is_premium => true,
	is_from_blockchain => true,
	colors => 'UniqueGiftColors'(),
	publisher_chat => 'Chat'()
}.

-doc """
Describes a service message about a regular gift that was sent or received.
  * `gift` - Information about the gift
  * `owned_gift_id` - Optional. Unique identifier of the received gift for the bot; only present for gifts received on behalf of business accounts
  * `convert_star_count` - Optional. Number of Telegram Stars that can be claimed by the receiver by converting the gift; omitted if conversion to Telegram Stars is impossible
  * `prepaid_upgrade_star_count` - Optional. Number of Telegram Stars that were prepaid for the ability to upgrade the gift
  * `is_upgrade_separate` - Optional. True, if the gift's upgrade was purchased after the gift was sent
  * `can_be_upgraded` - Optional. True, if the gift can be upgraded to a unique gift
  * `text` - Optional. Text of the message that was added to the gift
  * `entities` - Optional. Special entities that appear in the text
  * `is_private` - Optional. True, if the sender and gift text are shown only to the gift receiver; otherwise, everyone will be able to see them
  * `unique_gift_number` - Optional. Unique number reserved for this gift when upgraded. See the number field in UniqueGift
""".
-type 'GiftInfo'() :: #{
	gift := 'Gift'(),
	owned_gift_id => binary(),
	convert_star_count => integer(),
	prepaid_upgrade_star_count => integer(),
	is_upgrade_separate => true,
	can_be_upgraded => true,
	text => binary(),
	entities => nonempty_list('MessageEntity'()),
	is_private => true,
	unique_gift_number => integer()
}.

-doc """
Describes a service message about a unique gift that was sent or received.
  * `gift` - Information about the gift
  * `origin` - Origin of the gift. Currently, either “upgrade” for gifts upgraded from regular gifts, “transfer” for gifts transferred from other users or channels, “resale” for gifts bought from other users, “gifted_upgrade” for upgrades purchased after the gift was sent, or “offer” for gifts bought or sold through gift purchase offers
  * `last_resale_currency` - Optional. For gifts bought from other users, the currency in which the payment for the gift was done. Currently, one of “XTR” for Telegram Stars or “TON” for toncoins.
  * `last_resale_amount` - Optional. For gifts bought from other users, the price paid for the gift in either Telegram Stars or nanotoncoins
  * `owned_gift_id` - Optional. Unique identifier of the received gift for the bot; only present for gifts received on behalf of business accounts
  * `transfer_star_count` - Optional. Number of Telegram Stars that must be paid to transfer the gift; omitted if the bot cannot transfer the gift
  * `next_transfer_date` - Optional. Point in time (Unix timestamp) when the gift can be transferred. If it is in the past, then the gift can be transferred now
""".
-type 'UniqueGiftInfo'() :: #{
	gift := 'UniqueGift'(),
	origin := binary(),
	last_resale_currency => binary(),
	last_resale_amount => integer(),
	owned_gift_id => binary(),
	transfer_star_count => integer(),
	next_transfer_date => integer()
}.

-doc """
This object describes a gift received and owned by a user or a chat.  

""".
-type 'OwnedGift'() :: 'OwnedGiftRegular'() | 'OwnedGiftUnique'().

-doc """
Describes a regular gift owned by a user or a chat.
  * `type` - Type of the gift, always “regular”
  * `gift` - Information about the regular gift
  * `owned_gift_id` - Optional. Unique identifier of the gift for the bot; for gifts received on behalf of business accounts only
  * `sender_user` - Optional. Sender of the gift if it is a known user
  * `send_date` - Date the gift was sent in Unix time
  * `text` - Optional. Text of the message that was added to the gift
  * `entities` - Optional. Special entities that appear in the text
  * `is_private` - Optional. True, if the sender and gift text are shown only to the gift receiver; otherwise, everyone will be able to see them
  * `is_saved` - Optional. True, if the gift is displayed on the account's profile page; for gifts received on behalf of business accounts only
  * `can_be_upgraded` - Optional. True, if the gift can be upgraded to a unique gift; for gifts received on behalf of business accounts only
  * `was_refunded` - Optional. True, if the gift was refunded and isn't available anymore
  * `convert_star_count` - Optional. Number of Telegram Stars that can be claimed by the receiver instead of the gift; omitted if the gift cannot be converted to Telegram Stars; for gifts received on behalf of business accounts only
  * `prepaid_upgrade_star_count` - Optional. Number of Telegram Stars that were paid for the ability to upgrade the gift
  * `is_upgrade_separate` - Optional. True, if the gift's upgrade was purchased after the gift was sent; for gifts received on behalf of business accounts only
  * `unique_gift_number` - Optional. Unique number reserved for this gift when upgraded. See the number field in UniqueGift
""".
-type 'OwnedGiftRegular'() :: #{
	type := binary(),
	gift := 'Gift'(),
	owned_gift_id => binary(),
	sender_user => 'User'(),
	send_date := integer(),
	text => binary(),
	entities => nonempty_list('MessageEntity'()),
	is_private => true,
	is_saved => true,
	can_be_upgraded => true,
	was_refunded => true,
	convert_star_count => integer(),
	prepaid_upgrade_star_count => integer(),
	is_upgrade_separate => true,
	unique_gift_number => integer()
}.

-doc """
Describes a unique gift received and owned by a user or a chat.
  * `type` - Type of the gift, always “unique”
  * `gift` - Information about the unique gift
  * `owned_gift_id` - Optional. Unique identifier of the received gift for the bot; for gifts received on behalf of business accounts only
  * `sender_user` - Optional. Sender of the gift if it is a known user
  * `send_date` - Date the gift was sent in Unix time
  * `is_saved` - Optional. True, if the gift is displayed on the account's profile page; for gifts received on behalf of business accounts only
  * `can_be_transferred` - Optional. True, if the gift can be transferred to another owner; for gifts received on behalf of business accounts only
  * `transfer_star_count` - Optional. Number of Telegram Stars that must be paid to transfer the gift; omitted if the bot cannot transfer the gift
  * `next_transfer_date` - Optional. Point in time (Unix timestamp) when the gift can be transferred. If it is in the past, then the gift can be transferred now
""".
-type 'OwnedGiftUnique'() :: #{
	type := binary(),
	gift := 'UniqueGift'(),
	owned_gift_id => binary(),
	sender_user => 'User'(),
	send_date := integer(),
	is_saved => true,
	can_be_transferred => true,
	transfer_star_count => integer(),
	next_transfer_date => integer()
}.

-doc """
Contains the list of gifts received and owned by a user or a chat.
  * `total_count` - The total number of gifts owned by the user or the chat
  * `gifts` - The list of gifts
  * `next_offset` - Optional. Offset for the next request. If empty, then there are no more results
""".
-type 'OwnedGifts'() :: #{
	total_count := integer(),
	gifts := nonempty_list('OwnedGift'()),
	next_offset => binary()
}.

-doc """
This object describes the types of gifts that can be gifted to a user or a chat.
  * `unlimited_gifts` - True, if unlimited regular gifts are accepted
  * `limited_gifts` - True, if limited regular gifts are accepted
  * `unique_gifts` - True, if unique gifts or gifts that can be upgraded to unique for free are accepted
  * `premium_subscription` - True, if a Telegram Premium subscription is accepted
  * `gifts_from_channels` - True, if transfers of unique gifts from channels are accepted
""".
-type 'AcceptedGiftTypes'() :: #{
	unlimited_gifts := boolean(),
	limited_gifts := boolean(),
	unique_gifts := boolean(),
	premium_subscription := boolean(),
	gifts_from_channels := boolean()
}.

-doc """
Describes an amount of Telegram Stars.
  * `amount` - Integer amount of Telegram Stars, rounded to 0; can be negative
  * `nanostar_amount` - Optional. The number of 1/1000000000 shares of Telegram Stars; from -999999999 to 999999999; can be negative if and only if amount is non-positive
""".
-type 'StarAmount'() :: #{
	amount := integer(),
	nanostar_amount => integer()
}.

-doc """
This object represents a bot command.
  * `command` - Text of the command; 1-32 characters. Can contain only lowercase English letters, digits and underscores.
  * `description` - Description of the command; 1-256 characters.
""".
-type 'BotCommand'() :: #{
	command := binary(),
	description := binary()
}.

-doc """
This object represents the scope to which bot commands are applied.  
Currently, the following 7 scopes are supported:
""".
-type 'BotCommandScope'() :: 'BotCommandScopeDefault'() | 'BotCommandScopeAllPrivateChats'() | 'BotCommandScopeAllGroupChats'() | 'BotCommandScopeAllChatAdministrators'() | 'BotCommandScopeChat'() | 'BotCommandScopeChatAdministrators'() | 'BotCommandScopeChatMember'().

-doc """
Represents the default scope of bot commands.  
Default commands are used if no commands with a narrower scope are specified for the user.
  * `type` - Scope type, must be default
""".
-type 'BotCommandScopeDefault'() :: #{
	type := binary()
}.

-doc """
Represents the scope of bot commands, covering all private chats.
  * `type` - Scope type, must be all_private_chats
""".
-type 'BotCommandScopeAllPrivateChats'() :: #{
	type := binary()
}.

-doc """
Represents the scope of bot commands, covering all group and supergroup chats.
  * `type` - Scope type, must be all_group_chats
""".
-type 'BotCommandScopeAllGroupChats'() :: #{
	type := binary()
}.

-doc """
Represents the scope of bot commands, covering all group and supergroup chat administrators.
  * `type` - Scope type, must be all_chat_administrators
""".
-type 'BotCommandScopeAllChatAdministrators'() :: #{
	type := binary()
}.

-doc """
Represents the scope of bot commands, covering a specific chat.
  * `type` - Scope type, must be chat
  * `chat_id` - Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername). Channel direct messages chats and channel chats aren't supported.
""".
-type 'BotCommandScopeChat'() :: #{
	type := binary(),
	chat_id := integer() | binary()
}.

-doc """
Represents the scope of bot commands, covering all administrators of a specific group or supergroup chat.
  * `type` - Scope type, must be chat_administrators
  * `chat_id` - Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername). Channel direct messages chats and channel chats aren't supported.
""".
-type 'BotCommandScopeChatAdministrators'() :: #{
	type := binary(),
	chat_id := integer() | binary()
}.

-doc """
Represents the scope of bot commands, covering a specific member of a group or supergroup chat.
  * `type` - Scope type, must be chat_member
  * `chat_id` - Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername). Channel direct messages chats and channel chats aren't supported.
  * `user_id` - Unique identifier of the target user
""".
-type 'BotCommandScopeChatMember'() :: #{
	type := binary(),
	chat_id := integer() | binary(),
	user_id := integer()
}.

-doc """
This object represents the bot's name.
  * `name` - The bot's name
""".
-type 'BotName'() :: #{
	name := binary()
}.

-doc """
This object represents the bot's description.
  * `description` - The bot's description
""".
-type 'BotDescription'() :: #{
	description := binary()
}.

-doc """
This object represents the bot's short description.
  * `short_description` - The bot's short description
""".
-type 'BotShortDescription'() :: #{
	short_description := binary()
}.

-doc """
This object describes the bot's menu button in a private chat.  
It should be one of  
If a menu button other than MenuButtonDefault is set for a private chat, then it is applied in the chat.  
Otherwise the default menu button is applied.  
By default, the menu button opens the list of bot commands.
""".
-type 'MenuButton'() :: 'MenuButtonCommands'() | 'MenuButtonWebApp'() | 'MenuButtonDefault'().

-doc """
Represents a menu button, which opens the bot's list of commands.
  * `type` - Type of the button, must be commands
""".
-type 'MenuButtonCommands'() :: #{
	type := binary()
}.

-doc """
Represents a menu button, which launches a Web App.
  * `type` - Type of the button, must be web_app
  * `text` - Text on the button
  * `web_app` - Description of the Web App that will be launched when the user presses the button. The Web App will be able to send an arbitrary message on behalf of the user using the method answerWebAppQuery. Alternatively, a t.me link to a Web App of the bot can be specified in the object instead of the Web App's URL, in which case the Web App will be opened as if the user pressed the link.
""".
-type 'MenuButtonWebApp'() :: #{
	type := binary(),
	text := binary(),
	web_app := 'WebAppInfo'()
}.

-doc """
Describes that no specific value for the menu button was set.
  * `type` - Type of the button, must be default
""".
-type 'MenuButtonDefault'() :: #{
	type := binary()
}.

-doc """
This object describes the source of a chat boost.  

""".
-type 'ChatBoostSource'() :: 'ChatBoostSourcePremium'() | 'ChatBoostSourceGiftCode'() | 'ChatBoostSourceGiveaway'().

-doc """
The boost was obtained by subscribing to Telegram Premium or by gifting a Telegram Premium subscription to another user.
  * `source` - Source of the boost, always “premium”
  * `user` - User that boosted the chat
""".
-type 'ChatBoostSourcePremium'() :: #{
	source := binary(),
	user := 'User'()
}.

-doc """
The boost was obtained by the creation of Telegram Premium gift codes to boost a chat.  
Each such code boosts the chat 4 times for the duration of the corresponding Telegram Premium subscription.
  * `source` - Source of the boost, always “gift_code”
  * `user` - User for which the gift code was created
""".
-type 'ChatBoostSourceGiftCode'() :: #{
	source := binary(),
	user := 'User'()
}.

-doc """
The boost was obtained by the creation of a Telegram Premium or a Telegram Star giveaway.  
This boosts the chat 4 times for the duration of the corresponding Telegram Premium subscription for Telegram Premium giveaways and prize_star_count / 500 times for one year for Telegram Star giveaways.
  * `source` - Source of the boost, always “giveaway”
  * `giveaway_message_id` - Identifier of a message in the chat with the giveaway; the message could have been deleted already. May be 0 if the message isn't sent yet.
  * `user` - Optional. User that won the prize in the giveaway if any; for Telegram Premium giveaways only
  * `prize_star_count` - Optional. The number of Telegram Stars to be split between giveaway winners; for Telegram Star giveaways only
  * `is_unclaimed` - Optional. True, if the giveaway was completed, but there was no user to win the prize
""".
-type 'ChatBoostSourceGiveaway'() :: #{
	source := binary(),
	giveaway_message_id := integer(),
	user => 'User'(),
	prize_star_count => integer(),
	is_unclaimed => true
}.

-doc """
This object contains information about a chat boost.
  * `boost_id` - Unique identifier of the boost
  * `add_date` - Point in time (Unix timestamp) when the chat was boosted
  * `expiration_date` - Point in time (Unix timestamp) when the boost will automatically expire, unless the booster's Telegram Premium subscription is prolonged
  * `source` - Source of the added boost
""".
-type 'ChatBoost'() :: #{
	boost_id := binary(),
	add_date := integer(),
	expiration_date := integer(),
	source := 'ChatBoostSource'()
}.

-doc """
This object represents a boost added to a chat or changed.
  * `chat` - Chat which was boosted
  * `boost` - Information about the chat boost
""".
-type 'ChatBoostUpdated'() :: #{
	chat := 'Chat'(),
	boost := 'ChatBoost'()
}.

-doc """
This object represents a boost removed from a chat.
  * `chat` - Chat which was boosted
  * `boost_id` - Unique identifier of the boost
  * `remove_date` - Point in time (Unix timestamp) when the boost was removed
  * `source` - Source of the removed boost
""".
-type 'ChatBoostRemoved'() :: #{
	chat := 'Chat'(),
	boost_id := binary(),
	remove_date := integer(),
	source := 'ChatBoostSource'()
}.

-doc """
This object represents a list of boosts added to a chat by a user.
  * `boosts` - The list of boosts added to the chat by the user
""".
-type 'UserChatBoosts'() :: #{
	boosts := nonempty_list('ChatBoost'())
}.

-doc """
Represents the rights of a business bot.
  * `can_reply` - Optional. True, if the bot can send and edit messages in the private chats that had incoming messages in the last 24 hours
  * `can_read_messages` - Optional. True, if the bot can mark incoming private messages as read
  * `can_delete_sent_messages` - Optional. True, if the bot can delete messages sent by the bot
  * `can_delete_all_messages` - Optional. True, if the bot can delete all private messages in managed chats
  * `can_edit_name` - Optional. True, if the bot can edit the first and last name of the business account
  * `can_edit_bio` - Optional. True, if the bot can edit the bio of the business account
  * `can_edit_profile_photo` - Optional. True, if the bot can edit the profile photo of the business account
  * `can_edit_username` - Optional. True, if the bot can edit the username of the business account
  * `can_change_gift_settings` - Optional. True, if the bot can change the privacy settings pertaining to gifts for the business account
  * `can_view_gifts_and_stars` - Optional. True, if the bot can view gifts and the amount of Telegram Stars owned by the business account
  * `can_convert_gifts_to_stars` - Optional. True, if the bot can convert regular gifts owned by the business account to Telegram Stars
  * `can_transfer_and_upgrade_gifts` - Optional. True, if the bot can transfer and upgrade gifts owned by the business account
  * `can_transfer_stars` - Optional. True, if the bot can transfer Telegram Stars received by the business account to its own account, or use them to upgrade and transfer gifts
  * `can_manage_stories` - Optional. True, if the bot can post, edit and delete stories on behalf of the business account
""".
-type 'BusinessBotRights'() :: #{
	can_reply => true,
	can_read_messages => true,
	can_delete_sent_messages => true,
	can_delete_all_messages => true,
	can_edit_name => true,
	can_edit_bio => true,
	can_edit_profile_photo => true,
	can_edit_username => true,
	can_change_gift_settings => true,
	can_view_gifts_and_stars => true,
	can_convert_gifts_to_stars => true,
	can_transfer_and_upgrade_gifts => true,
	can_transfer_stars => true,
	can_manage_stories => true
}.

-doc """
Describes the connection of the bot with a business account.
  * `id` - Unique identifier of the business connection
  * `user` - Business account user that created the business connection
  * `user_chat_id` - Identifier of a private chat with the user who created the business connection. This number may have more than 32 significant bits and some programming languages may have difficulty/silent defects in interpreting it. But it has at most 52 significant bits, so a 64-bit integer or double-precision float type are safe for storing this identifier.
  * `date` - Date the connection was established in Unix time
  * `rights` - Optional. Rights of the business bot
  * `is_enabled` - True, if the connection is active
""".
-type 'BusinessConnection'() :: #{
	id := binary(),
	user := 'User'(),
	user_chat_id := integer(),
	date := integer(),
	rights => 'BusinessBotRights'(),
	is_enabled := boolean()
}.

-doc """
This object is received when messages are deleted from a connected business account.
  * `business_connection_id` - Unique identifier of the business connection
  * `chat` - Information about a chat in the business account. The bot may not have access to the chat or the corresponding user.
  * `message_ids` - The list of identifiers of deleted messages in the chat of the business account
""".
-type 'BusinessMessagesDeleted'() :: #{
	business_connection_id := binary(),
	chat := 'Chat'(),
	message_ids := nonempty_list(integer())
}.

-doc """
Describes why a request was unsuccessful.
  * `migrate_to_chat_id` - Optional. The group has been migrated to a supergroup with the specified identifier. This number may have more than 32 significant bits and some programming languages may have difficulty/silent defects in interpreting it. But it has at most 52 significant bits, so a signed 64-bit integer or double-precision float type are safe for storing this identifier.
  * `retry_after` - Optional. In case of exceeding flood control, the number of seconds left to wait before the request can be repeated
""".
-type 'ResponseParameters'() :: #{
	migrate_to_chat_id => integer(),
	retry_after => integer()
}.

-doc """
This object represents the content of a media message to be sent.  
It should be one of
""".
-type 'InputMedia'() :: 'InputMediaAnimation'() | 'InputMediaDocument'() | 'InputMediaAudio'() | 'InputMediaPhoto'() | 'InputMediaVideo'().

-doc """
Represents a photo to be sent.
  * `type` - Type of the result, must be photo
  * `media` - File to send. Pass a file_id to send a file that exists on the Telegram servers (recommended), pass an HTTP URL for Telegram to get a file from the Internet, or pass “attach://<file_attach_name>” to upload a new one using multipart/form-data under <file_attach_name> name. More information on Sending Files »
  * `caption` - Optional. Caption of the photo to be sent, 0-1024 characters after entities parsing
  * `parse_mode` - Optional. Mode for parsing entities in the photo caption. See formatting options for more details.
  * `caption_entities` - Optional. List of special entities that appear in the caption, which can be specified instead of parse_mode
  * `show_caption_above_media` - Optional. Pass True, if the caption must be shown above the message media
  * `has_spoiler` - Optional. Pass True if the photo needs to be covered with a spoiler animation
""".
-type 'InputMediaPhoto'() :: #{
	type := binary(),
	media := binary(),
	caption => binary(),
	parse_mode => binary(),
	caption_entities => nonempty_list('MessageEntity'()),
	show_caption_above_media => boolean(),
	has_spoiler => boolean()
}.

-doc """
Represents a video to be sent.
  * `type` - Type of the result, must be video
  * `media` - File to send. Pass a file_id to send a file that exists on the Telegram servers (recommended), pass an HTTP URL for Telegram to get a file from the Internet, or pass “attach://<file_attach_name>” to upload a new one using multipart/form-data under <file_attach_name> name. More information on Sending Files »
  * `thumbnail` - Optional. Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://<file_attach_name>” if the thumbnail was uploaded using multipart/form-data under <file_attach_name>. More information on Sending Files »
  * `cover` - Optional. Cover for the video in the message. Pass a file_id to send a file that exists on the Telegram servers (recommended), pass an HTTP URL for Telegram to get a file from the Internet, or pass “attach://<file_attach_name>” to upload a new one using multipart/form-data under <file_attach_name> name. More information on Sending Files »
  * `start_timestamp` - Optional. Start timestamp for the video in the message
  * `caption` - Optional. Caption of the video to be sent, 0-1024 characters after entities parsing
  * `parse_mode` - Optional. Mode for parsing entities in the video caption. See formatting options for more details.
  * `caption_entities` - Optional. List of special entities that appear in the caption, which can be specified instead of parse_mode
  * `show_caption_above_media` - Optional. Pass True, if the caption must be shown above the message media
  * `width` - Optional. Video width
  * `height` - Optional. Video height
  * `duration` - Optional. Video duration in seconds
  * `supports_streaming` - Optional. Pass True if the uploaded video is suitable for streaming
  * `has_spoiler` - Optional. Pass True if the video needs to be covered with a spoiler animation
""".
-type 'InputMediaVideo'() :: #{
	type := binary(),
	media := binary(),
	thumbnail => binary(),
	cover => binary(),
	start_timestamp => integer(),
	caption => binary(),
	parse_mode => binary(),
	caption_entities => nonempty_list('MessageEntity'()),
	show_caption_above_media => boolean(),
	width => integer(),
	height => integer(),
	duration => integer(),
	supports_streaming => boolean(),
	has_spoiler => boolean()
}.

-doc """
Represents an animation file (GIF or H.264/MPEG-4 AVC video without sound) to be sent.
  * `type` - Type of the result, must be animation
  * `media` - File to send. Pass a file_id to send a file that exists on the Telegram servers (recommended), pass an HTTP URL for Telegram to get a file from the Internet, or pass “attach://<file_attach_name>” to upload a new one using multipart/form-data under <file_attach_name> name. More information on Sending Files »
  * `thumbnail` - Optional. Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://<file_attach_name>” if the thumbnail was uploaded using multipart/form-data under <file_attach_name>. More information on Sending Files »
  * `caption` - Optional. Caption of the animation to be sent, 0-1024 characters after entities parsing
  * `parse_mode` - Optional. Mode for parsing entities in the animation caption. See formatting options for more details.
  * `caption_entities` - Optional. List of special entities that appear in the caption, which can be specified instead of parse_mode
  * `show_caption_above_media` - Optional. Pass True, if the caption must be shown above the message media
  * `width` - Optional. Animation width
  * `height` - Optional. Animation height
  * `duration` - Optional. Animation duration in seconds
  * `has_spoiler` - Optional. Pass True if the animation needs to be covered with a spoiler animation
""".
-type 'InputMediaAnimation'() :: #{
	type := binary(),
	media := binary(),
	thumbnail => binary(),
	caption => binary(),
	parse_mode => binary(),
	caption_entities => nonempty_list('MessageEntity'()),
	show_caption_above_media => boolean(),
	width => integer(),
	height => integer(),
	duration => integer(),
	has_spoiler => boolean()
}.

-doc """
Represents an audio file to be treated as music to be sent.
  * `type` - Type of the result, must be audio
  * `media` - File to send. Pass a file_id to send a file that exists on the Telegram servers (recommended), pass an HTTP URL for Telegram to get a file from the Internet, or pass “attach://<file_attach_name>” to upload a new one using multipart/form-data under <file_attach_name> name. More information on Sending Files »
  * `thumbnail` - Optional. Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://<file_attach_name>” if the thumbnail was uploaded using multipart/form-data under <file_attach_name>. More information on Sending Files »
  * `caption` - Optional. Caption of the audio to be sent, 0-1024 characters after entities parsing
  * `parse_mode` - Optional. Mode for parsing entities in the audio caption. See formatting options for more details.
  * `caption_entities` - Optional. List of special entities that appear in the caption, which can be specified instead of parse_mode
  * `duration` - Optional. Duration of the audio in seconds
  * `performer` - Optional. Performer of the audio
  * `title` - Optional. Title of the audio
""".
-type 'InputMediaAudio'() :: #{
	type := binary(),
	media := binary(),
	thumbnail => binary(),
	caption => binary(),
	parse_mode => binary(),
	caption_entities => nonempty_list('MessageEntity'()),
	duration => integer(),
	performer => binary(),
	title => binary()
}.

-doc """
Represents a general file to be sent.
  * `type` - Type of the result, must be document
  * `media` - File to send. Pass a file_id to send a file that exists on the Telegram servers (recommended), pass an HTTP URL for Telegram to get a file from the Internet, or pass “attach://<file_attach_name>” to upload a new one using multipart/form-data under <file_attach_name> name. More information on Sending Files »
  * `thumbnail` - Optional. Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://<file_attach_name>” if the thumbnail was uploaded using multipart/form-data under <file_attach_name>. More information on Sending Files »
  * `caption` - Optional. Caption of the document to be sent, 0-1024 characters after entities parsing
  * `parse_mode` - Optional. Mode for parsing entities in the document caption. See formatting options for more details.
  * `caption_entities` - Optional. List of special entities that appear in the caption, which can be specified instead of parse_mode
  * `disable_content_type_detection` - Optional. Disables automatic server-side content type detection for files uploaded using multipart/form-data. Always True, if the document is sent as part of an album.
""".
-type 'InputMediaDocument'() :: #{
	type := binary(),
	media := binary(),
	thumbnail => binary(),
	caption => binary(),
	parse_mode => binary(),
	caption_entities => nonempty_list('MessageEntity'()),
	disable_content_type_detection => boolean()
}.

-doc """
This object represents the contents of a file to be uploaded.  
Must be posted using multipart/form-data in the usual way that files are uploaded via the browser.
""".
-type 'InputFile'() :: multipart_file().

-doc """
This object describes the paid media to be sent.  

""".
-type 'InputPaidMedia'() :: 'InputPaidMediaPhoto'() | 'InputPaidMediaVideo'().

-doc """
The paid media to send is a photo.
  * `type` - Type of the media, must be photo
  * `media` - File to send. Pass a file_id to send a file that exists on the Telegram servers (recommended), pass an HTTP URL for Telegram to get a file from the Internet, or pass “attach://<file_attach_name>” to upload a new one using multipart/form-data under <file_attach_name> name. More information on Sending Files »
""".
-type 'InputPaidMediaPhoto'() :: #{
	type := binary(),
	media := binary()
}.

-doc """
The paid media to send is a video.
  * `type` - Type of the media, must be video
  * `media` - File to send. Pass a file_id to send a file that exists on the Telegram servers (recommended), pass an HTTP URL for Telegram to get a file from the Internet, or pass “attach://<file_attach_name>” to upload a new one using multipart/form-data under <file_attach_name> name. More information on Sending Files »
  * `thumbnail` - Optional. Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://<file_attach_name>” if the thumbnail was uploaded using multipart/form-data under <file_attach_name>. More information on Sending Files »
  * `cover` - Optional. Cover for the video in the message. Pass a file_id to send a file that exists on the Telegram servers (recommended), pass an HTTP URL for Telegram to get a file from the Internet, or pass “attach://<file_attach_name>” to upload a new one using multipart/form-data under <file_attach_name> name. More information on Sending Files »
  * `start_timestamp` - Optional. Start timestamp for the video in the message
  * `width` - Optional. Video width
  * `height` - Optional. Video height
  * `duration` - Optional. Video duration in seconds
  * `supports_streaming` - Optional. Pass True if the uploaded video is suitable for streaming
""".
-type 'InputPaidMediaVideo'() :: #{
	type := binary(),
	media := binary(),
	thumbnail => binary(),
	cover => binary(),
	start_timestamp => integer(),
	width => integer(),
	height => integer(),
	duration => integer(),
	supports_streaming => boolean()
}.

-doc """
This object describes a profile photo to set.  

""".
-type 'InputProfilePhoto'() :: 'InputProfilePhotoStatic'() | 'InputProfilePhotoAnimated'().

-doc """
A static profile photo in the .JPG format.
  * `type` - Type of the profile photo, must be static
  * `photo` - The static profile photo. Profile photos can't be reused and can only be uploaded as a new file, so you can pass “attach://<file_attach_name>” if the photo was uploaded using multipart/form-data under <file_attach_name>. More information on Sending Files »
""".
-type 'InputProfilePhotoStatic'() :: #{
	type := binary(),
	photo := binary()
}.

-doc """
An animated profile photo in the MPEG4 format.
  * `type` - Type of the profile photo, must be animated
  * `animation` - The animated profile photo. Profile photos can't be reused and can only be uploaded as a new file, so you can pass “attach://<file_attach_name>” if the photo was uploaded using multipart/form-data under <file_attach_name>. More information on Sending Files »
  * `main_frame_timestamp` - Optional. Timestamp in seconds of the frame that will be used as the static profile photo. Defaults to 0.0.
""".
-type 'InputProfilePhotoAnimated'() :: #{
	type := binary(),
	animation := binary(),
	main_frame_timestamp => float()
}.

-doc """
This object describes the content of a story to post.  

""".
-type 'InputStoryContent'() :: 'InputStoryContentPhoto'() | 'InputStoryContentVideo'().

-doc """
Describes a photo to post as a story.
  * `type` - Type of the content, must be photo
  * `photo` - The photo to post as a story. The photo must be of the size 1080x1920 and must not exceed 10 MB. The photo can't be reused and can only be uploaded as a new file, so you can pass “attach://<file_attach_name>” if the photo was uploaded using multipart/form-data under <file_attach_name>. More information on Sending Files »
""".
-type 'InputStoryContentPhoto'() :: #{
	type := binary(),
	photo := binary()
}.

-doc """
Describes a video to post as a story.
  * `type` - Type of the content, must be video
  * `video` - The video to post as a story. The video must be of the size 720x1280, streamable, encoded with H.265 codec, with key frames added each second in the MPEG4 format, and must not exceed 30 MB. The video can't be reused and can only be uploaded as a new file, so you can pass “attach://<file_attach_name>” if the video was uploaded using multipart/form-data under <file_attach_name>. More information on Sending Files »
  * `duration` - Optional. Precise duration of the video in seconds; 0-60
  * `cover_frame_timestamp` - Optional. Timestamp in seconds of the frame that will be used as the static cover for the story. Defaults to 0.0.
  * `is_animation` - Optional. Pass True if the video has no sound
""".
-type 'InputStoryContentVideo'() :: #{
	type := binary(),
	video := binary(),
	duration => float(),
	cover_frame_timestamp => float(),
	is_animation => boolean()
}.

-doc """
This object represents a sticker.
  * `file_id` - Identifier for this file, which can be used to download or reuse the file
  * `file_unique_id` - Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  * `type` - Type of the sticker, currently one of “regular”, “mask”, “custom_emoji”. The type of the sticker is independent from its format, which is determined by the fields is_animated and is_video.
  * `width` - Sticker width
  * `height` - Sticker height
  * `is_animated` - True, if the sticker is animated
  * `is_video` - True, if the sticker is a video sticker
  * `thumbnail` - Optional. Sticker thumbnail in the .WEBP or .JPG format
  * `emoji` - Optional. Emoji associated with the sticker
  * `set_name` - Optional. Name of the sticker set to which the sticker belongs
  * `premium_animation` - Optional. For premium regular stickers, premium animation for the sticker
  * `mask_position` - Optional. For mask stickers, the position where the mask should be placed
  * `custom_emoji_id` - Optional. For custom emoji stickers, unique identifier of the custom emoji
  * `needs_repainting` - Optional. True, if the sticker must be repainted to a text color in messages, the color of the Telegram Premium badge in emoji status, white color on chat photos, or another appropriate color in other places
  * `file_size` - Optional. File size in bytes
""".
-type 'Sticker'() :: #{
	file_id := binary(),
	file_unique_id := binary(),
	type := binary(),
	width := integer(),
	height := integer(),
	is_animated := boolean(),
	is_video := boolean(),
	thumbnail => 'PhotoSize'(),
	emoji => binary(),
	set_name => binary(),
	premium_animation => 'File'(),
	mask_position => 'MaskPosition'(),
	custom_emoji_id => binary(),
	needs_repainting => true,
	file_size => integer()
}.

-doc """
This object represents a sticker set.
  * `name` - Sticker set name
  * `title` - Sticker set title
  * `sticker_type` - Type of stickers in the set, currently one of “regular”, “mask”, “custom_emoji”
  * `stickers` - List of all set stickers
  * `thumbnail` - Optional. Sticker set thumbnail in the .WEBP, .TGS, or .WEBM format
""".
-type 'StickerSet'() :: #{
	name := binary(),
	title := binary(),
	sticker_type := binary(),
	stickers := nonempty_list('Sticker'()),
	thumbnail => 'PhotoSize'()
}.

-doc """
This object describes the position on faces where a mask should be placed by default.
  * `point` - The part of the face relative to which the mask should be placed. One of “forehead”, “eyes”, “mouth”, or “chin”.
  * `x_shift` - Shift by X-axis measured in widths of the mask scaled to the face size, from left to right. For example, choosing -1.0 will place mask just to the left of the default mask position.
  * `y_shift` - Shift by Y-axis measured in heights of the mask scaled to the face size, from top to bottom. For example, 1.0 will place the mask just below the default mask position.
  * `scale` - Mask scaling coefficient. For example, 2.0 means double size.
""".
-type 'MaskPosition'() :: #{
	point := binary(),
	x_shift := float(),
	y_shift := float(),
	scale := float()
}.

-doc """
This object describes a sticker to be added to a sticker set.
  * `sticker` - The added sticker. Pass a file_id as a String to send a file that already exists on the Telegram servers, pass an HTTP URL as a String for Telegram to get a file from the Internet, or pass “attach://<file_attach_name>” to upload a new file using multipart/form-data under <file_attach_name> name. Animated and video stickers can't be uploaded via HTTP URL. More information on Sending Files »
  * `format` - Format of the added sticker, must be one of “static” for a .WEBP or .PNG image, “animated” for a .TGS animation, “video” for a .WEBM video
  * `emoji_list` - List of 1-20 emoji associated with the sticker
  * `mask_position` - Optional. Position where the mask should be placed on faces. For “mask” stickers only.
  * `keywords` - Optional. List of 0-20 search keywords for the sticker with total length of up to 64 characters. For “regular” and “custom_emoji” stickers only.
""".
-type 'InputSticker'() :: #{
	sticker := binary(),
	format := binary(),
	emoji_list := nonempty_list(binary()),
	mask_position => 'MaskPosition'(),
	keywords => nonempty_list(binary())
}.

-doc """
This object represents an incoming inline query.  
When the user sends an empty query, your bot could return some default or trending results.
  * `id` - Unique identifier for this query
  * `from` - Sender
  * `query` - Text of the query (up to 256 characters)
  * `offset` - Offset of the results to be returned, can be controlled by the bot
  * `chat_type` - Optional. Type of the chat from which the inline query was sent. Can be either “sender” for a private chat with the inline query sender, “private”, “group”, “supergroup”, or “channel”. The chat type should be always known for requests sent from official clients and most third-party clients, unless the request was sent from a secret chat
  * `location` - Optional. Sender location, only for bots that request user location
""".
-type 'InlineQuery'() :: #{
	id := binary(),
	from := 'User'(),
	query := binary(),
	offset := binary(),
	chat_type => binary(),
	location => 'Location'()
}.

-doc """
This object represents a button to be shown above inline query results.  
You must use exactly one of the optional fields.
  * `text` - Label text on the button
  * `web_app` - Optional. Description of the Web App that will be launched when the user presses the button. The Web App will be able to switch back to the inline mode using the method switchInlineQuery inside the Web App.
  * `start_parameter` - Optional. Deep-linking parameter for the /start message sent to the bot when a user presses the button. 1-64 characters, only A-Z, a-z, 0-9, _ and - are allowed.

Example: An inline bot that sends YouTube videos can ask the user to connect the bot to their YouTube account to adapt search results accordingly. To do this, it displays a 'Connect your YouTube account' button above the results, or even before showing any. The user presses the button, switches to a private chat with the bot and, in doing so, passes a start parameter that instructs the bot to return an OAuth link. Once done, the bot can offer a switch_inline button so that the user can easily return to the chat where they wanted to use the bot's inline capabilities.
""".
-type 'InlineQueryResultsButton'() :: #{
	text := binary(),
	web_app => 'WebAppInfo'(),
	start_parameter => binary()
}.

-doc """
This object represents one result of an inline query.  
Telegram clients currently support results of the following 20 types:  
Note: All URLs passed in inline query results will be available to end users and therefore must be assumed to be public.
""".
-type 'InlineQueryResult'() :: 'InlineQueryResultCachedAudio'() | 'InlineQueryResultCachedDocument'() | 'InlineQueryResultCachedGif'() | 'InlineQueryResultCachedMpeg4Gif'() | 'InlineQueryResultCachedPhoto'() | 'InlineQueryResultCachedSticker'() | 'InlineQueryResultCachedVideo'() | 'InlineQueryResultCachedVoice'() | 'InlineQueryResultArticle'() | 'InlineQueryResultAudio'() | 'InlineQueryResultContact'() | 'InlineQueryResultGame'() | 'InlineQueryResultDocument'() | 'InlineQueryResultGif'() | 'InlineQueryResultLocation'() | 'InlineQueryResultMpeg4Gif'() | 'InlineQueryResultPhoto'() | 'InlineQueryResultVenue'() | 'InlineQueryResultVideo'() | 'InlineQueryResultVoice'().

-doc """
Represents a link to an article or web page.
  * `type` - Type of the result, must be article
  * `id` - Unique identifier for this result, 1-64 Bytes
  * `title` - Title of the result
  * `input_message_content` - Content of the message to be sent
  * `reply_markup` - Optional. Inline keyboard attached to the message
  * `url` - Optional. URL of the result
  * `description` - Optional. Short description of the result
  * `thumbnail_url` - Optional. Url of the thumbnail for the result
  * `thumbnail_width` - Optional. Thumbnail width
  * `thumbnail_height` - Optional. Thumbnail height
""".
-type 'InlineQueryResultArticle'() :: #{
	type := binary(),
	id := binary(),
	title := binary(),
	input_message_content := 'InputMessageContent'(),
	reply_markup => 'InlineKeyboardMarkup'(),
	url => binary(),
	description => binary(),
	thumbnail_url => binary(),
	thumbnail_width => integer(),
	thumbnail_height => integer()
}.

-doc """
Represents a link to a photo.  
By default, this photo will be sent by the user with optional caption.  
Alternatively, you can use input_message_content to send a message with the specified content instead of the photo.
  * `type` - Type of the result, must be photo
  * `id` - Unique identifier for this result, 1-64 bytes
  * `photo_url` - A valid URL of the photo. Photo must be in JPEG format. Photo size must not exceed 5MB
  * `thumbnail_url` - URL of the thumbnail for the photo
  * `photo_width` - Optional. Width of the photo
  * `photo_height` - Optional. Height of the photo
  * `title` - Optional. Title for the result
  * `description` - Optional. Short description of the result
  * `caption` - Optional. Caption of the photo to be sent, 0-1024 characters after entities parsing
  * `parse_mode` - Optional. Mode for parsing entities in the photo caption. See formatting options for more details.
  * `caption_entities` - Optional. List of special entities that appear in the caption, which can be specified instead of parse_mode
  * `show_caption_above_media` - Optional. Pass True, if the caption must be shown above the message media
  * `reply_markup` - Optional. Inline keyboard attached to the message
  * `input_message_content` - Optional. Content of the message to be sent instead of the photo
""".
-type 'InlineQueryResultPhoto'() :: #{
	type := binary(),
	id := binary(),
	photo_url := binary(),
	thumbnail_url := binary(),
	photo_width => integer(),
	photo_height => integer(),
	title => binary(),
	description => binary(),
	caption => binary(),
	parse_mode => binary(),
	caption_entities => nonempty_list('MessageEntity'()),
	show_caption_above_media => boolean(),
	reply_markup => 'InlineKeyboardMarkup'(),
	input_message_content => 'InputMessageContent'()
}.

-doc """
Represents a link to an animated GIF file.  
By default, this animated GIF file will be sent by the user with optional caption.  
Alternatively, you can use input_message_content to send a message with the specified content instead of the animation.
  * `type` - Type of the result, must be gif
  * `id` - Unique identifier for this result, 1-64 bytes
  * `gif_url` - A valid URL for the GIF file
  * `gif_width` - Optional. Width of the GIF
  * `gif_height` - Optional. Height of the GIF
  * `gif_duration` - Optional. Duration of the GIF in seconds
  * `thumbnail_url` - URL of the static (JPEG or GIF) or animated (MPEG4) thumbnail for the result
  * `thumbnail_mime_type` - Optional. MIME type of the thumbnail, must be one of “image/jpeg”, “image/gif”, or “video/mp4”. Defaults to “image/jpeg”
  * `title` - Optional. Title for the result
  * `caption` - Optional. Caption of the GIF file to be sent, 0-1024 characters after entities parsing
  * `parse_mode` - Optional. Mode for parsing entities in the caption. See formatting options for more details.
  * `caption_entities` - Optional. List of special entities that appear in the caption, which can be specified instead of parse_mode
  * `show_caption_above_media` - Optional. Pass True, if the caption must be shown above the message media
  * `reply_markup` - Optional. Inline keyboard attached to the message
  * `input_message_content` - Optional. Content of the message to be sent instead of the GIF animation
""".
-type 'InlineQueryResultGif'() :: #{
	type := binary(),
	id := binary(),
	gif_url := binary(),
	gif_width => integer(),
	gif_height => integer(),
	gif_duration => integer(),
	thumbnail_url := binary(),
	thumbnail_mime_type => binary(),
	title => binary(),
	caption => binary(),
	parse_mode => binary(),
	caption_entities => nonempty_list('MessageEntity'()),
	show_caption_above_media => boolean(),
	reply_markup => 'InlineKeyboardMarkup'(),
	input_message_content => 'InputMessageContent'()
}.

-doc """
Represents a link to a video animation (H.264/MPEG-4 AVC video without sound).  
By default, this animated MPEG-4 file will be sent by the user with optional caption.  
Alternatively, you can use input_message_content to send a message with the specified content instead of the animation.
  * `type` - Type of the result, must be mpeg4_gif
  * `id` - Unique identifier for this result, 1-64 bytes
  * `mpeg4_url` - A valid URL for the MPEG4 file
  * `mpeg4_width` - Optional. Video width
  * `mpeg4_height` - Optional. Video height
  * `mpeg4_duration` - Optional. Video duration in seconds
  * `thumbnail_url` - URL of the static (JPEG or GIF) or animated (MPEG4) thumbnail for the result
  * `thumbnail_mime_type` - Optional. MIME type of the thumbnail, must be one of “image/jpeg”, “image/gif”, or “video/mp4”. Defaults to “image/jpeg”
  * `title` - Optional. Title for the result
  * `caption` - Optional. Caption of the MPEG-4 file to be sent, 0-1024 characters after entities parsing
  * `parse_mode` - Optional. Mode for parsing entities in the caption. See formatting options for more details.
  * `caption_entities` - Optional. List of special entities that appear in the caption, which can be specified instead of parse_mode
  * `show_caption_above_media` - Optional. Pass True, if the caption must be shown above the message media
  * `reply_markup` - Optional. Inline keyboard attached to the message
  * `input_message_content` - Optional. Content of the message to be sent instead of the video animation
""".
-type 'InlineQueryResultMpeg4Gif'() :: #{
	type := binary(),
	id := binary(),
	mpeg4_url := binary(),
	mpeg4_width => integer(),
	mpeg4_height => integer(),
	mpeg4_duration => integer(),
	thumbnail_url := binary(),
	thumbnail_mime_type => binary(),
	title => binary(),
	caption => binary(),
	parse_mode => binary(),
	caption_entities => nonempty_list('MessageEntity'()),
	show_caption_above_media => boolean(),
	reply_markup => 'InlineKeyboardMarkup'(),
	input_message_content => 'InputMessageContent'()
}.

-doc """
Represents a link to a page containing an embedded video player or a video file.  
By default, this video file will be sent by the user with an optional caption.  
Alternatively, you can use input_message_content to send a message with the specified content instead of the video.  
If an InlineQueryResultVideo message contains an embedded video (e.g., YouTube), you must replace its content using input_message_content.
  * `type` - Type of the result, must be video
  * `id` - Unique identifier for this result, 1-64 bytes
  * `video_url` - A valid URL for the embedded video player or video file
  * `mime_type` - MIME type of the content of the video URL, “text/html” or “video/mp4”
  * `thumbnail_url` - URL of the thumbnail (JPEG only) for the video
  * `title` - Title for the result
  * `caption` - Optional. Caption of the video to be sent, 0-1024 characters after entities parsing
  * `parse_mode` - Optional. Mode for parsing entities in the video caption. See formatting options for more details.
  * `caption_entities` - Optional. List of special entities that appear in the caption, which can be specified instead of parse_mode
  * `show_caption_above_media` - Optional. Pass True, if the caption must be shown above the message media
  * `video_width` - Optional. Video width
  * `video_height` - Optional. Video height
  * `video_duration` - Optional. Video duration in seconds
  * `description` - Optional. Short description of the result
  * `reply_markup` - Optional. Inline keyboard attached to the message
  * `input_message_content` - Optional. Content of the message to be sent instead of the video. This field is required if InlineQueryResultVideo is used to send an HTML-page as a result (e.g., a YouTube video).
""".
-type 'InlineQueryResultVideo'() :: #{
	type := binary(),
	id := binary(),
	video_url := binary(),
	mime_type := binary(),
	thumbnail_url := binary(),
	title := binary(),
	caption => binary(),
	parse_mode => binary(),
	caption_entities => nonempty_list('MessageEntity'()),
	show_caption_above_media => boolean(),
	video_width => integer(),
	video_height => integer(),
	video_duration => integer(),
	description => binary(),
	reply_markup => 'InlineKeyboardMarkup'(),
	input_message_content => 'InputMessageContent'()
}.

-doc """
Represents a link to an MP3 audio file.  
By default, this audio file will be sent by the user.  
Alternatively, you can use input_message_content to send a message with the specified content instead of the audio.
  * `type` - Type of the result, must be audio
  * `id` - Unique identifier for this result, 1-64 bytes
  * `audio_url` - A valid URL for the audio file
  * `title` - Title
  * `caption` - Optional. Caption, 0-1024 characters after entities parsing
  * `parse_mode` - Optional. Mode for parsing entities in the audio caption. See formatting options for more details.
  * `caption_entities` - Optional. List of special entities that appear in the caption, which can be specified instead of parse_mode
  * `performer` - Optional. Performer
  * `audio_duration` - Optional. Audio duration in seconds
  * `reply_markup` - Optional. Inline keyboard attached to the message
  * `input_message_content` - Optional. Content of the message to be sent instead of the audio
""".
-type 'InlineQueryResultAudio'() :: #{
	type := binary(),
	id := binary(),
	audio_url := binary(),
	title := binary(),
	caption => binary(),
	parse_mode => binary(),
	caption_entities => nonempty_list('MessageEntity'()),
	performer => binary(),
	audio_duration => integer(),
	reply_markup => 'InlineKeyboardMarkup'(),
	input_message_content => 'InputMessageContent'()
}.

-doc """
Represents a link to a voice recording in an .OGG container encoded with OPUS.  
By default, this voice recording will be sent by the user.  
Alternatively, you can use input_message_content to send a message with the specified content instead of the the voice message.
  * `type` - Type of the result, must be voice
  * `id` - Unique identifier for this result, 1-64 bytes
  * `voice_url` - A valid URL for the voice recording
  * `title` - Recording title
  * `caption` - Optional. Caption, 0-1024 characters after entities parsing
  * `parse_mode` - Optional. Mode for parsing entities in the voice message caption. See formatting options for more details.
  * `caption_entities` - Optional. List of special entities that appear in the caption, which can be specified instead of parse_mode
  * `voice_duration` - Optional. Recording duration in seconds
  * `reply_markup` - Optional. Inline keyboard attached to the message
  * `input_message_content` - Optional. Content of the message to be sent instead of the voice recording
""".
-type 'InlineQueryResultVoice'() :: #{
	type := binary(),
	id := binary(),
	voice_url := binary(),
	title := binary(),
	caption => binary(),
	parse_mode => binary(),
	caption_entities => nonempty_list('MessageEntity'()),
	voice_duration => integer(),
	reply_markup => 'InlineKeyboardMarkup'(),
	input_message_content => 'InputMessageContent'()
}.

-doc """
Represents a link to a file.  
By default, this file will be sent by the user with an optional caption.  
Alternatively, you can use input_message_content to send a message with the specified content instead of the file.  
Currently, only .PDF and .ZIP files can be sent using this method.
  * `type` - Type of the result, must be document
  * `id` - Unique identifier for this result, 1-64 bytes
  * `title` - Title for the result
  * `caption` - Optional. Caption of the document to be sent, 0-1024 characters after entities parsing
  * `parse_mode` - Optional. Mode for parsing entities in the document caption. See formatting options for more details.
  * `caption_entities` - Optional. List of special entities that appear in the caption, which can be specified instead of parse_mode
  * `document_url` - A valid URL for the file
  * `mime_type` - MIME type of the content of the file, either “application/pdf” or “application/zip”
  * `description` - Optional. Short description of the result
  * `reply_markup` - Optional. Inline keyboard attached to the message
  * `input_message_content` - Optional. Content of the message to be sent instead of the file
  * `thumbnail_url` - Optional. URL of the thumbnail (JPEG only) for the file
  * `thumbnail_width` - Optional. Thumbnail width
  * `thumbnail_height` - Optional. Thumbnail height
""".
-type 'InlineQueryResultDocument'() :: #{
	type := binary(),
	id := binary(),
	title := binary(),
	caption => binary(),
	parse_mode => binary(),
	caption_entities => nonempty_list('MessageEntity'()),
	document_url := binary(),
	mime_type := binary(),
	description => binary(),
	reply_markup => 'InlineKeyboardMarkup'(),
	input_message_content => 'InputMessageContent'(),
	thumbnail_url => binary(),
	thumbnail_width => integer(),
	thumbnail_height => integer()
}.

-doc """
Represents a location on a map.  
By default, the location will be sent by the user.  
Alternatively, you can use input_message_content to send a message with the specified content instead of the location.
  * `type` - Type of the result, must be location
  * `id` - Unique identifier for this result, 1-64 Bytes
  * `latitude` - Location latitude in degrees
  * `longitude` - Location longitude in degrees
  * `title` - Location title
  * `horizontal_accuracy` - Optional. The radius of uncertainty for the location, measured in meters; 0-1500
  * `live_period` - Optional. Period in seconds during which the location can be updated, should be between 60 and 86400, or 0x7FFFFFFF for live locations that can be edited indefinitely.
  * `heading` - Optional. For live locations, a direction in which the user is moving, in degrees. Must be between 1 and 360 if specified.
  * `proximity_alert_radius` - Optional. For live locations, a maximum distance for proximity alerts about approaching another chat member, in meters. Must be between 1 and 100000 if specified.
  * `reply_markup` - Optional. Inline keyboard attached to the message
  * `input_message_content` - Optional. Content of the message to be sent instead of the location
  * `thumbnail_url` - Optional. Url of the thumbnail for the result
  * `thumbnail_width` - Optional. Thumbnail width
  * `thumbnail_height` - Optional. Thumbnail height
""".
-type 'InlineQueryResultLocation'() :: #{
	type := binary(),
	id := binary(),
	latitude := float(),
	longitude := float(),
	title := binary(),
	horizontal_accuracy => float(),
	live_period => integer(),
	heading => integer(),
	proximity_alert_radius => integer(),
	reply_markup => 'InlineKeyboardMarkup'(),
	input_message_content => 'InputMessageContent'(),
	thumbnail_url => binary(),
	thumbnail_width => integer(),
	thumbnail_height => integer()
}.

-doc """
Represents a venue.  
By default, the venue will be sent by the user.  
Alternatively, you can use input_message_content to send a message with the specified content instead of the venue.
  * `type` - Type of the result, must be venue
  * `id` - Unique identifier for this result, 1-64 Bytes
  * `latitude` - Latitude of the venue location in degrees
  * `longitude` - Longitude of the venue location in degrees
  * `title` - Title of the venue
  * `address` - Address of the venue
  * `foursquare_id` - Optional. Foursquare identifier of the venue if known
  * `foursquare_type` - Optional. Foursquare type of the venue, if known. (For example, “arts_entertainment/default”, “arts_entertainment/aquarium” or “food/icecream”.)
  * `google_place_id` - Optional. Google Places identifier of the venue
  * `google_place_type` - Optional. Google Places type of the venue. (See supported types.)
  * `reply_markup` - Optional. Inline keyboard attached to the message
  * `input_message_content` - Optional. Content of the message to be sent instead of the venue
  * `thumbnail_url` - Optional. Url of the thumbnail for the result
  * `thumbnail_width` - Optional. Thumbnail width
  * `thumbnail_height` - Optional. Thumbnail height
""".
-type 'InlineQueryResultVenue'() :: #{
	type := binary(),
	id := binary(),
	latitude := float(),
	longitude := float(),
	title := binary(),
	address := binary(),
	foursquare_id => binary(),
	foursquare_type => binary(),
	google_place_id => binary(),
	google_place_type => binary(),
	reply_markup => 'InlineKeyboardMarkup'(),
	input_message_content => 'InputMessageContent'(),
	thumbnail_url => binary(),
	thumbnail_width => integer(),
	thumbnail_height => integer()
}.

-doc """
Represents a contact with a phone number.  
By default, this contact will be sent by the user.  
Alternatively, you can use input_message_content to send a message with the specified content instead of the contact.
  * `type` - Type of the result, must be contact
  * `id` - Unique identifier for this result, 1-64 Bytes
  * `phone_number` - Contact's phone number
  * `first_name` - Contact's first name
  * `last_name` - Optional. Contact's last name
  * `vcard` - Optional. Additional data about the contact in the form of a vCard, 0-2048 bytes
  * `reply_markup` - Optional. Inline keyboard attached to the message
  * `input_message_content` - Optional. Content of the message to be sent instead of the contact
  * `thumbnail_url` - Optional. Url of the thumbnail for the result
  * `thumbnail_width` - Optional. Thumbnail width
  * `thumbnail_height` - Optional. Thumbnail height
""".
-type 'InlineQueryResultContact'() :: #{
	type := binary(),
	id := binary(),
	phone_number := binary(),
	first_name := binary(),
	last_name => binary(),
	vcard => binary(),
	reply_markup => 'InlineKeyboardMarkup'(),
	input_message_content => 'InputMessageContent'(),
	thumbnail_url => binary(),
	thumbnail_width => integer(),
	thumbnail_height => integer()
}.

-doc """
Represents a Game.
  * `type` - Type of the result, must be game
  * `id` - Unique identifier for this result, 1-64 bytes
  * `game_short_name` - Short name of the game
  * `reply_markup` - Optional. Inline keyboard attached to the message
""".
-type 'InlineQueryResultGame'() :: #{
	type := binary(),
	id := binary(),
	game_short_name := binary(),
	reply_markup => 'InlineKeyboardMarkup'()
}.

-doc """
Represents a link to a photo stored on the Telegram servers.  
By default, this photo will be sent by the user with an optional caption.  
Alternatively, you can use input_message_content to send a message with the specified content instead of the photo.
  * `type` - Type of the result, must be photo
  * `id` - Unique identifier for this result, 1-64 bytes
  * `photo_file_id` - A valid file identifier of the photo
  * `title` - Optional. Title for the result
  * `description` - Optional. Short description of the result
  * `caption` - Optional. Caption of the photo to be sent, 0-1024 characters after entities parsing
  * `parse_mode` - Optional. Mode for parsing entities in the photo caption. See formatting options for more details.
  * `caption_entities` - Optional. List of special entities that appear in the caption, which can be specified instead of parse_mode
  * `show_caption_above_media` - Optional. Pass True, if the caption must be shown above the message media
  * `reply_markup` - Optional. Inline keyboard attached to the message
  * `input_message_content` - Optional. Content of the message to be sent instead of the photo
""".
-type 'InlineQueryResultCachedPhoto'() :: #{
	type := binary(),
	id := binary(),
	photo_file_id := binary(),
	title => binary(),
	description => binary(),
	caption => binary(),
	parse_mode => binary(),
	caption_entities => nonempty_list('MessageEntity'()),
	show_caption_above_media => boolean(),
	reply_markup => 'InlineKeyboardMarkup'(),
	input_message_content => 'InputMessageContent'()
}.

-doc """
Represents a link to an animated GIF file stored on the Telegram servers.  
By default, this animated GIF file will be sent by the user with an optional caption.  
Alternatively, you can use input_message_content to send a message with specified content instead of the animation.
  * `type` - Type of the result, must be gif
  * `id` - Unique identifier for this result, 1-64 bytes
  * `gif_file_id` - A valid file identifier for the GIF file
  * `title` - Optional. Title for the result
  * `caption` - Optional. Caption of the GIF file to be sent, 0-1024 characters after entities parsing
  * `parse_mode` - Optional. Mode for parsing entities in the caption. See formatting options for more details.
  * `caption_entities` - Optional. List of special entities that appear in the caption, which can be specified instead of parse_mode
  * `show_caption_above_media` - Optional. Pass True, if the caption must be shown above the message media
  * `reply_markup` - Optional. Inline keyboard attached to the message
  * `input_message_content` - Optional. Content of the message to be sent instead of the GIF animation
""".
-type 'InlineQueryResultCachedGif'() :: #{
	type := binary(),
	id := binary(),
	gif_file_id := binary(),
	title => binary(),
	caption => binary(),
	parse_mode => binary(),
	caption_entities => nonempty_list('MessageEntity'()),
	show_caption_above_media => boolean(),
	reply_markup => 'InlineKeyboardMarkup'(),
	input_message_content => 'InputMessageContent'()
}.

-doc """
Represents a link to a video animation (H.264/MPEG-4 AVC video without sound) stored on the Telegram servers.  
By default, this animated MPEG-4 file will be sent by the user with an optional caption.  
Alternatively, you can use input_message_content to send a message with the specified content instead of the animation.
  * `type` - Type of the result, must be mpeg4_gif
  * `id` - Unique identifier for this result, 1-64 bytes
  * `mpeg4_file_id` - A valid file identifier for the MPEG4 file
  * `title` - Optional. Title for the result
  * `caption` - Optional. Caption of the MPEG-4 file to be sent, 0-1024 characters after entities parsing
  * `parse_mode` - Optional. Mode for parsing entities in the caption. See formatting options for more details.
  * `caption_entities` - Optional. List of special entities that appear in the caption, which can be specified instead of parse_mode
  * `show_caption_above_media` - Optional. Pass True, if the caption must be shown above the message media
  * `reply_markup` - Optional. Inline keyboard attached to the message
  * `input_message_content` - Optional. Content of the message to be sent instead of the video animation
""".
-type 'InlineQueryResultCachedMpeg4Gif'() :: #{
	type := binary(),
	id := binary(),
	mpeg4_file_id := binary(),
	title => binary(),
	caption => binary(),
	parse_mode => binary(),
	caption_entities => nonempty_list('MessageEntity'()),
	show_caption_above_media => boolean(),
	reply_markup => 'InlineKeyboardMarkup'(),
	input_message_content => 'InputMessageContent'()
}.

-doc """
Represents a link to a sticker stored on the Telegram servers.  
By default, this sticker will be sent by the user.  
Alternatively, you can use input_message_content to send a message with the specified content instead of the sticker.
  * `type` - Type of the result, must be sticker
  * `id` - Unique identifier for this result, 1-64 bytes
  * `sticker_file_id` - A valid file identifier of the sticker
  * `reply_markup` - Optional. Inline keyboard attached to the message
  * `input_message_content` - Optional. Content of the message to be sent instead of the sticker
""".
-type 'InlineQueryResultCachedSticker'() :: #{
	type := binary(),
	id := binary(),
	sticker_file_id := binary(),
	reply_markup => 'InlineKeyboardMarkup'(),
	input_message_content => 'InputMessageContent'()
}.

-doc """
Represents a link to a file stored on the Telegram servers.  
By default, this file will be sent by the user with an optional caption.  
Alternatively, you can use input_message_content to send a message with the specified content instead of the file.
  * `type` - Type of the result, must be document
  * `id` - Unique identifier for this result, 1-64 bytes
  * `title` - Title for the result
  * `document_file_id` - A valid file identifier for the file
  * `description` - Optional. Short description of the result
  * `caption` - Optional. Caption of the document to be sent, 0-1024 characters after entities parsing
  * `parse_mode` - Optional. Mode for parsing entities in the document caption. See formatting options for more details.
  * `caption_entities` - Optional. List of special entities that appear in the caption, which can be specified instead of parse_mode
  * `reply_markup` - Optional. Inline keyboard attached to the message
  * `input_message_content` - Optional. Content of the message to be sent instead of the file
""".
-type 'InlineQueryResultCachedDocument'() :: #{
	type := binary(),
	id := binary(),
	title := binary(),
	document_file_id := binary(),
	description => binary(),
	caption => binary(),
	parse_mode => binary(),
	caption_entities => nonempty_list('MessageEntity'()),
	reply_markup => 'InlineKeyboardMarkup'(),
	input_message_content => 'InputMessageContent'()
}.

-doc """
Represents a link to a video file stored on the Telegram servers.  
By default, this video file will be sent by the user with an optional caption.  
Alternatively, you can use input_message_content to send a message with the specified content instead of the video.
  * `type` - Type of the result, must be video
  * `id` - Unique identifier for this result, 1-64 bytes
  * `video_file_id` - A valid file identifier for the video file
  * `title` - Title for the result
  * `description` - Optional. Short description of the result
  * `caption` - Optional. Caption of the video to be sent, 0-1024 characters after entities parsing
  * `parse_mode` - Optional. Mode for parsing entities in the video caption. See formatting options for more details.
  * `caption_entities` - Optional. List of special entities that appear in the caption, which can be specified instead of parse_mode
  * `show_caption_above_media` - Optional. Pass True, if the caption must be shown above the message media
  * `reply_markup` - Optional. Inline keyboard attached to the message
  * `input_message_content` - Optional. Content of the message to be sent instead of the video
""".
-type 'InlineQueryResultCachedVideo'() :: #{
	type := binary(),
	id := binary(),
	video_file_id := binary(),
	title := binary(),
	description => binary(),
	caption => binary(),
	parse_mode => binary(),
	caption_entities => nonempty_list('MessageEntity'()),
	show_caption_above_media => boolean(),
	reply_markup => 'InlineKeyboardMarkup'(),
	input_message_content => 'InputMessageContent'()
}.

-doc """
Represents a link to a voice message stored on the Telegram servers.  
By default, this voice message will be sent by the user.  
Alternatively, you can use input_message_content to send a message with the specified content instead of the voice message.
  * `type` - Type of the result, must be voice
  * `id` - Unique identifier for this result, 1-64 bytes
  * `voice_file_id` - A valid file identifier for the voice message
  * `title` - Voice message title
  * `caption` - Optional. Caption, 0-1024 characters after entities parsing
  * `parse_mode` - Optional. Mode for parsing entities in the voice message caption. See formatting options for more details.
  * `caption_entities` - Optional. List of special entities that appear in the caption, which can be specified instead of parse_mode
  * `reply_markup` - Optional. Inline keyboard attached to the message
  * `input_message_content` - Optional. Content of the message to be sent instead of the voice message
""".
-type 'InlineQueryResultCachedVoice'() :: #{
	type := binary(),
	id := binary(),
	voice_file_id := binary(),
	title := binary(),
	caption => binary(),
	parse_mode => binary(),
	caption_entities => nonempty_list('MessageEntity'()),
	reply_markup => 'InlineKeyboardMarkup'(),
	input_message_content => 'InputMessageContent'()
}.

-doc """
Represents a link to an MP3 audio file stored on the Telegram servers.  
By default, this audio file will be sent by the user.  
Alternatively, you can use input_message_content to send a message with the specified content instead of the audio.
  * `type` - Type of the result, must be audio
  * `id` - Unique identifier for this result, 1-64 bytes
  * `audio_file_id` - A valid file identifier for the audio file
  * `caption` - Optional. Caption, 0-1024 characters after entities parsing
  * `parse_mode` - Optional. Mode for parsing entities in the audio caption. See formatting options for more details.
  * `caption_entities` - Optional. List of special entities that appear in the caption, which can be specified instead of parse_mode
  * `reply_markup` - Optional. Inline keyboard attached to the message
  * `input_message_content` - Optional. Content of the message to be sent instead of the audio
""".
-type 'InlineQueryResultCachedAudio'() :: #{
	type := binary(),
	id := binary(),
	audio_file_id := binary(),
	caption => binary(),
	parse_mode => binary(),
	caption_entities => nonempty_list('MessageEntity'()),
	reply_markup => 'InlineKeyboardMarkup'(),
	input_message_content => 'InputMessageContent'()
}.

-doc """
This object represents the content of a message to be sent as a result of an inline query.  
Telegram clients currently support the following 5 types:
""".
-type 'InputMessageContent'() :: 'InputTextMessageContent'() | 'InputLocationMessageContent'() | 'InputVenueMessageContent'() | 'InputContactMessageContent'() | 'InputInvoiceMessageContent'().

-doc """
Represents the content of a text message to be sent as the result of an inline query.
  * `message_text` - Text of the message to be sent, 1-4096 characters
  * `parse_mode` - Optional. Mode for parsing entities in the message text. See formatting options for more details.
  * `entities` - Optional. List of special entities that appear in message text, which can be specified instead of parse_mode
  * `link_preview_options` - Optional. Link preview generation options for the message
""".
-type 'InputTextMessageContent'() :: #{
	message_text := binary(),
	parse_mode => binary(),
	entities => nonempty_list('MessageEntity'()),
	link_preview_options => 'LinkPreviewOptions'()
}.

-doc """
Represents the content of a location message to be sent as the result of an inline query.
  * `latitude` - Latitude of the location in degrees
  * `longitude` - Longitude of the location in degrees
  * `horizontal_accuracy` - Optional. The radius of uncertainty for the location, measured in meters; 0-1500
  * `live_period` - Optional. Period in seconds during which the location can be updated, should be between 60 and 86400, or 0x7FFFFFFF for live locations that can be edited indefinitely.
  * `heading` - Optional. For live locations, a direction in which the user is moving, in degrees. Must be between 1 and 360 if specified.
  * `proximity_alert_radius` - Optional. For live locations, a maximum distance for proximity alerts about approaching another chat member, in meters. Must be between 1 and 100000 if specified.
""".
-type 'InputLocationMessageContent'() :: #{
	latitude := float(),
	longitude := float(),
	horizontal_accuracy => float(),
	live_period => integer(),
	heading => integer(),
	proximity_alert_radius => integer()
}.

-doc """
Represents the content of a venue message to be sent as the result of an inline query.
  * `latitude` - Latitude of the venue in degrees
  * `longitude` - Longitude of the venue in degrees
  * `title` - Name of the venue
  * `address` - Address of the venue
  * `foursquare_id` - Optional. Foursquare identifier of the venue, if known
  * `foursquare_type` - Optional. Foursquare type of the venue, if known. (For example, “arts_entertainment/default”, “arts_entertainment/aquarium” or “food/icecream”.)
  * `google_place_id` - Optional. Google Places identifier of the venue
  * `google_place_type` - Optional. Google Places type of the venue. (See supported types.)
""".
-type 'InputVenueMessageContent'() :: #{
	latitude := float(),
	longitude := float(),
	title := binary(),
	address := binary(),
	foursquare_id => binary(),
	foursquare_type => binary(),
	google_place_id => binary(),
	google_place_type => binary()
}.

-doc """
Represents the content of a contact message to be sent as the result of an inline query.
  * `phone_number` - Contact's phone number
  * `first_name` - Contact's first name
  * `last_name` - Optional. Contact's last name
  * `vcard` - Optional. Additional data about the contact in the form of a vCard, 0-2048 bytes
""".
-type 'InputContactMessageContent'() :: #{
	phone_number := binary(),
	first_name := binary(),
	last_name => binary(),
	vcard => binary()
}.

-doc """
Represents the content of an invoice message to be sent as the result of an inline query.
  * `title` - Product name, 1-32 characters
  * `description` - Product description, 1-255 characters
  * `payload` - Bot-defined invoice payload, 1-128 bytes. This will not be displayed to the user, use it for your internal processes.
  * `provider_token` - Optional. Payment provider token, obtained via @BotFather. Pass an empty string for payments in Telegram Stars.
  * `currency` - Three-letter ISO 4217 currency code, see more on currencies. Pass “XTR” for payments in Telegram Stars.
  * `prices` - Price breakdown, a JSON-serialized list of components (e.g. product price, tax, discount, delivery cost, delivery tax, bonus, etc.). Must contain exactly one item for payments in Telegram Stars.
  * `max_tip_amount` - Optional. The maximum accepted amount for tips in the smallest units of the currency (integer, not float/double). For example, for a maximum tip of US$ 1.45 pass max_tip_amount = 145. See the exp parameter in currencies.json, it shows the number of digits past the decimal point for each currency (2 for the majority of currencies). Defaults to 0. Not supported for payments in Telegram Stars.
  * `suggested_tip_amounts` - Optional. A JSON-serialized array of suggested amounts of tip in the smallest units of the currency (integer, not float/double). At most 4 suggested tip amounts can be specified. The suggested tip amounts must be positive, passed in a strictly increased order and must not exceed max_tip_amount.
  * `provider_data` - Optional. A JSON-serialized object for data about the invoice, which will be shared with the payment provider. A detailed description of the required fields should be provided by the payment provider.
  * `photo_url` - Optional. URL of the product photo for the invoice. Can be a photo of the goods or a marketing image for a service.
  * `photo_size` - Optional. Photo size in bytes
  * `photo_width` - Optional. Photo width
  * `photo_height` - Optional. Photo height
  * `need_name` - Optional. Pass True if you require the user's full name to complete the order. Ignored for payments in Telegram Stars.
  * `need_phone_number` - Optional. Pass True if you require the user's phone number to complete the order. Ignored for payments in Telegram Stars.
  * `need_email` - Optional. Pass True if you require the user's email address to complete the order. Ignored for payments in Telegram Stars.
  * `need_shipping_address` - Optional. Pass True if you require the user's shipping address to complete the order. Ignored for payments in Telegram Stars.
  * `send_phone_number_to_provider` - Optional. Pass True if the user's phone number should be sent to the provider. Ignored for payments in Telegram Stars.
  * `send_email_to_provider` - Optional. Pass True if the user's email address should be sent to the provider. Ignored for payments in Telegram Stars.
  * `is_flexible` - Optional. Pass True if the final price depends on the shipping method. Ignored for payments in Telegram Stars.
""".
-type 'InputInvoiceMessageContent'() :: #{
	title := binary(),
	description := binary(),
	payload := binary(),
	provider_token => binary(),
	currency := binary(),
	prices := nonempty_list('LabeledPrice'()),
	max_tip_amount => integer(),
	suggested_tip_amounts => nonempty_list(integer()),
	provider_data => binary(),
	photo_url => binary(),
	photo_size => integer(),
	photo_width => integer(),
	photo_height => integer(),
	need_name => boolean(),
	need_phone_number => boolean(),
	need_email => boolean(),
	need_shipping_address => boolean(),
	send_phone_number_to_provider => boolean(),
	send_email_to_provider => boolean(),
	is_flexible => boolean()
}.

-doc """
Represents a result of an inline query that was chosen by the user and sent to their chat partner.  
Note: It is necessary to enable inline feedback via @BotFather in order to receive these objects in updates.
  * `result_id` - The unique identifier for the result that was chosen
  * `from` - The user that chose the result
  * `location` - Optional. Sender location, only for bots that require user location
  * `inline_message_id` - Optional. Identifier of the sent inline message. Available only if there is an inline keyboard attached to the message. Will be also received in callback queries and can be used to edit the message.
  * `query` - The query that was used to obtain the result
""".
-type 'ChosenInlineResult'() :: #{
	result_id := binary(),
	from := 'User'(),
	location => 'Location'(),
	inline_message_id => binary(),
	query := binary()
}.

-doc """
Describes an inline message sent by a Web App on behalf of a user.
  * `inline_message_id` - Optional. Identifier of the sent inline message. Available only if there is an inline keyboard attached to the message.
""".
-type 'SentWebAppMessage'() :: #{
	inline_message_id => binary()
}.

-doc """
Describes an inline message to be sent by a user of a Mini App.
  * `id` - Unique identifier of the prepared message
  * `expiration_date` - Expiration date of the prepared message, in Unix time. Expired prepared messages can no longer be used
""".
-type 'PreparedInlineMessage'() :: #{
	id := binary(),
	expiration_date := integer()
}.

-doc """
This object represents a portion of the price for goods or services.
  * `label` - Portion label
  * `amount` - Price of the product in the smallest units of the currency (integer, not float/double). For example, for a price of US$ 1.45 pass amount = 145. See the exp parameter in currencies.json, it shows the number of digits past the decimal point for each currency (2 for the majority of currencies).
""".
-type 'LabeledPrice'() :: #{
	label := binary(),
	amount := integer()
}.

-doc """
This object contains basic information about an invoice.
  * `title` - Product name
  * `description` - Product description
  * `start_parameter` - Unique bot deep-linking parameter that can be used to generate this invoice
  * `currency` - Three-letter ISO 4217 currency code, or “XTR” for payments in Telegram Stars
  * `total_amount` - Total price in the smallest units of the currency (integer, not float/double). For example, for a price of US$ 1.45 pass amount = 145. See the exp parameter in currencies.json, it shows the number of digits past the decimal point for each currency (2 for the majority of currencies).
""".
-type 'Invoice'() :: #{
	title := binary(),
	description := binary(),
	start_parameter := binary(),
	currency := binary(),
	total_amount := integer()
}.

-doc """
This object represents a shipping address.
  * `country_code` - Two-letter ISO 3166-1 alpha-2 country code
  * `state` - State, if applicable
  * `city` - City
  * `street_line1` - First line for the address
  * `street_line2` - Second line for the address
  * `post_code` - Address post code
""".
-type 'ShippingAddress'() :: #{
	country_code := binary(),
	state := binary(),
	city := binary(),
	street_line1 := binary(),
	street_line2 := binary(),
	post_code := binary()
}.

-doc """
This object represents information about an order.
  * `name` - Optional. User name
  * `phone_number` - Optional. User's phone number
  * `email` - Optional. User email
  * `shipping_address` - Optional. User shipping address
""".
-type 'OrderInfo'() :: #{
	name => binary(),
	phone_number => binary(),
	email => binary(),
	shipping_address => 'ShippingAddress'()
}.

-doc """
This object represents one shipping option.
  * `id` - Shipping option identifier
  * `title` - Option title
  * `prices` - List of price portions
""".
-type 'ShippingOption'() :: #{
	id := binary(),
	title := binary(),
	prices := nonempty_list('LabeledPrice'())
}.

-doc """
This object contains basic information about a successful payment.  
Note that if the buyer initiates a chargeback with the relevant payment provider following this transaction, the funds may be debited from your balance.  
This is outside of Telegram's control.
  * `currency` - Three-letter ISO 4217 currency code, or “XTR” for payments in Telegram Stars
  * `total_amount` - Total price in the smallest units of the currency (integer, not float/double). For example, for a price of US$ 1.45 pass amount = 145. See the exp parameter in currencies.json, it shows the number of digits past the decimal point for each currency (2 for the majority of currencies).
  * `invoice_payload` - Bot-specified invoice payload
  * `subscription_expiration_date` - Optional. Expiration date of the subscription, in Unix time; for recurring payments only
  * `is_recurring` - Optional. True, if the payment is a recurring payment for a subscription
  * `is_first_recurring` - Optional. True, if the payment is the first payment for a subscription
  * `shipping_option_id` - Optional. Identifier of the shipping option chosen by the user
  * `order_info` - Optional. Order information provided by the user
  * `telegram_payment_charge_id` - Telegram payment identifier
  * `provider_payment_charge_id` - Provider payment identifier
""".
-type 'SuccessfulPayment'() :: #{
	currency := binary(),
	total_amount := integer(),
	invoice_payload := binary(),
	subscription_expiration_date => integer(),
	is_recurring => true,
	is_first_recurring => true,
	shipping_option_id => binary(),
	order_info => 'OrderInfo'(),
	telegram_payment_charge_id := binary(),
	provider_payment_charge_id := binary()
}.

-doc """
This object contains basic information about a refunded payment.
  * `currency` - Three-letter ISO 4217 currency code, or “XTR” for payments in Telegram Stars. Currently, always “XTR”
  * `total_amount` - Total refunded price in the smallest units of the currency (integer, not float/double). For example, for a price of US$ 1.45, total_amount = 145. See the exp parameter in currencies.json, it shows the number of digits past the decimal point for each currency (2 for the majority of currencies).
  * `invoice_payload` - Bot-specified invoice payload
  * `telegram_payment_charge_id` - Telegram payment identifier
  * `provider_payment_charge_id` - Optional. Provider payment identifier
""".
-type 'RefundedPayment'() :: #{
	currency := binary(),
	total_amount := integer(),
	invoice_payload := binary(),
	telegram_payment_charge_id := binary(),
	provider_payment_charge_id => binary()
}.

-doc """
This object contains information about an incoming shipping query.
  * `id` - Unique query identifier
  * `from` - User who sent the query
  * `invoice_payload` - Bot-specified invoice payload
  * `shipping_address` - User specified shipping address
""".
-type 'ShippingQuery'() :: #{
	id := binary(),
	from := 'User'(),
	invoice_payload := binary(),
	shipping_address := 'ShippingAddress'()
}.

-doc """
This object contains information about an incoming pre-checkout query.
  * `id` - Unique query identifier
  * `from` - User who sent the query
  * `currency` - Three-letter ISO 4217 currency code, or “XTR” for payments in Telegram Stars
  * `total_amount` - Total price in the smallest units of the currency (integer, not float/double). For example, for a price of US$ 1.45 pass amount = 145. See the exp parameter in currencies.json, it shows the number of digits past the decimal point for each currency (2 for the majority of currencies).
  * `invoice_payload` - Bot-specified invoice payload
  * `shipping_option_id` - Optional. Identifier of the shipping option chosen by the user
  * `order_info` - Optional. Order information provided by the user
""".
-type 'PreCheckoutQuery'() :: #{
	id := binary(),
	from := 'User'(),
	currency := binary(),
	total_amount := integer(),
	invoice_payload := binary(),
	shipping_option_id => binary(),
	order_info => 'OrderInfo'()
}.

-doc """
This object contains information about a paid media purchase.
  * `from` - User who purchased the media
  * `paid_media_payload` - Bot-specified paid media payload
""".
-type 'PaidMediaPurchased'() :: #{
	from := 'User'(),
	paid_media_payload := binary()
}.

-doc """
This object describes the state of a revenue withdrawal operation.  

""".
-type 'RevenueWithdrawalState'() :: 'RevenueWithdrawalStatePending'() | 'RevenueWithdrawalStateSucceeded'() | 'RevenueWithdrawalStateFailed'().

-doc """
The withdrawal is in progress.
  * `type` - Type of the state, always “pending”
""".
-type 'RevenueWithdrawalStatePending'() :: #{
	type := binary()
}.

-doc """
The withdrawal succeeded.
  * `type` - Type of the state, always “succeeded”
  * `date` - Date the withdrawal was completed in Unix time
  * `url` - An HTTPS URL that can be used to see transaction details
""".
-type 'RevenueWithdrawalStateSucceeded'() :: #{
	type := binary(),
	date := integer(),
	url := binary()
}.

-doc """
The withdrawal failed and the transaction was refunded.
  * `type` - Type of the state, always “failed”
""".
-type 'RevenueWithdrawalStateFailed'() :: #{
	type := binary()
}.

-doc """
Contains information about the affiliate that received a commission via this transaction.
  * `affiliate_user` - Optional. The bot or the user that received an affiliate commission if it was received by a bot or a user
  * `affiliate_chat` - Optional. The chat that received an affiliate commission if it was received by a chat
  * `commission_per_mille` - The number of Telegram Stars received by the affiliate for each 1000 Telegram Stars received by the bot from referred users
  * `amount` - Integer amount of Telegram Stars received by the affiliate from the transaction, rounded to 0; can be negative for refunds
  * `nanostar_amount` - Optional. The number of 1/1000000000 shares of Telegram Stars received by the affiliate; from -999999999 to 999999999; can be negative for refunds
""".
-type 'AffiliateInfo'() :: #{
	affiliate_user => 'User'(),
	affiliate_chat => 'Chat'(),
	commission_per_mille := integer(),
	amount := integer(),
	nanostar_amount => integer()
}.

-doc """
This object describes the source of a transaction, or its recipient for outgoing transactions.  

""".
-type 'TransactionPartner'() :: 'TransactionPartnerUser'() | 'TransactionPartnerChat'() | 'TransactionPartnerAffiliateProgram'() | 'TransactionPartnerFragment'() | 'TransactionPartnerTelegramAds'() | 'TransactionPartnerTelegramApi'() | 'TransactionPartnerOther'().

-doc """
Describes a transaction with a user.
  * `type` - Type of the transaction partner, always “user”
  * `transaction_type` - Type of the transaction, currently one of “invoice_payment” for payments via invoices, “paid_media_payment” for payments for paid media, “gift_purchase” for gifts sent by the bot, “premium_purchase” for Telegram Premium subscriptions gifted by the bot, “business_account_transfer” for direct transfers from managed business accounts
  * `user` - Information about the user
  * `affiliate` - Optional. Information about the affiliate that received a commission via this transaction. Can be available only for “invoice_payment” and “paid_media_payment” transactions.
  * `invoice_payload` - Optional. Bot-specified invoice payload. Can be available only for “invoice_payment” transactions.
  * `subscription_period` - Optional. The duration of the paid subscription. Can be available only for “invoice_payment” transactions.
  * `paid_media` - Optional. Information about the paid media bought by the user; for “paid_media_payment” transactions only
  * `paid_media_payload` - Optional. Bot-specified paid media payload. Can be available only for “paid_media_payment” transactions.
  * `gift` - Optional. The gift sent to the user by the bot; for “gift_purchase” transactions only
  * `premium_subscription_duration` - Optional. Number of months the gifted Telegram Premium subscription will be active for; for “premium_purchase” transactions only
""".
-type 'TransactionPartnerUser'() :: #{
	type := binary(),
	transaction_type := binary(),
	user := 'User'(),
	affiliate => 'AffiliateInfo'(),
	invoice_payload => binary(),
	subscription_period => integer(),
	paid_media => nonempty_list('PaidMedia'()),
	paid_media_payload => binary(),
	gift => 'Gift'(),
	premium_subscription_duration => integer()
}.

-doc """
Describes a transaction with a chat.
  * `type` - Type of the transaction partner, always “chat”
  * `chat` - Information about the chat
  * `gift` - Optional. The gift sent to the chat by the bot
""".
-type 'TransactionPartnerChat'() :: #{
	type := binary(),
	chat := 'Chat'(),
	gift => 'Gift'()
}.

-doc """
Describes the affiliate program that issued the affiliate commission received via this transaction.
  * `type` - Type of the transaction partner, always “affiliate_program”
  * `sponsor_user` - Optional. Information about the bot that sponsored the affiliate program
  * `commission_per_mille` - The number of Telegram Stars received by the bot for each 1000 Telegram Stars received by the affiliate program sponsor from referred users
""".
-type 'TransactionPartnerAffiliateProgram'() :: #{
	type := binary(),
	sponsor_user => 'User'(),
	commission_per_mille := integer()
}.

-doc """
Describes a withdrawal transaction with Fragment.
  * `type` - Type of the transaction partner, always “fragment”
  * `withdrawal_state` - Optional. State of the transaction if the transaction is outgoing
""".
-type 'TransactionPartnerFragment'() :: #{
	type := binary(),
	withdrawal_state => 'RevenueWithdrawalState'()
}.

-doc """
Describes a withdrawal transaction to the Telegram Ads platform.
  * `type` - Type of the transaction partner, always “telegram_ads”
""".
-type 'TransactionPartnerTelegramAds'() :: #{
	type := binary()
}.

-doc """
Describes a transaction with payment for paid broadcasting.
  * `type` - Type of the transaction partner, always “telegram_api”
  * `request_count` - The number of successful requests that exceeded regular limits and were therefore billed
""".
-type 'TransactionPartnerTelegramApi'() :: #{
	type := binary(),
	request_count := integer()
}.

-doc """
Describes a transaction with an unknown source or recipient.
  * `type` - Type of the transaction partner, always “other”
""".
-type 'TransactionPartnerOther'() :: #{
	type := binary()
}.

-doc """
Describes a Telegram Star transaction.  
Note that if the buyer initiates a chargeback with the payment provider from whom they acquired Stars (e.g., Apple, Google) following this transaction, the refunded Stars will be deducted from the bot's balance.  
This is outside of Telegram's control.
  * `id` - Unique identifier of the transaction. Coincides with the identifier of the original transaction for refund transactions. Coincides with SuccessfulPayment.telegram_payment_charge_id for successful incoming payments from users.
  * `amount` - Integer amount of Telegram Stars transferred by the transaction
  * `nanostar_amount` - Optional. The number of 1/1000000000 shares of Telegram Stars transferred by the transaction; from 0 to 999999999
  * `date` - Date the transaction was created in Unix time
  * `source` - Optional. Source of an incoming transaction (e.g., a user purchasing goods or services, Fragment refunding a failed withdrawal). Only for incoming transactions
  * `receiver` - Optional. Receiver of an outgoing transaction (e.g., a user for a purchase refund, Fragment for a withdrawal). Only for outgoing transactions
""".
-type 'StarTransaction'() :: #{
	id := binary(),
	amount := integer(),
	nanostar_amount => integer(),
	date := integer(),
	source => 'TransactionPartner'(),
	receiver => 'TransactionPartner'()
}.

-doc """
Contains a list of Telegram Star transactions.
  * `transactions` - The list of transactions
""".
-type 'StarTransactions'() :: #{
	transactions := nonempty_list('StarTransaction'())
}.

-doc """
Describes Telegram Passport data shared with the bot by the user.
  * `data` - Array with information about documents and other Telegram Passport elements that was shared with the bot
  * `credentials` - Encrypted credentials required to decrypt the data
""".
-type 'PassportData'() :: #{
	data := nonempty_list('EncryptedPassportElement'()),
	credentials := 'EncryptedCredentials'()
}.

-doc """
This object represents a file uploaded to Telegram Passport.  
Currently all Telegram Passport files are in JPEG format when decrypted and don't exceed 10MB.
  * `file_id` - Identifier for this file, which can be used to download or reuse the file
  * `file_unique_id` - Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  * `file_size` - File size in bytes
  * `file_date` - Unix time when the file was uploaded
""".
-type 'PassportFile'() :: #{
	file_id := binary(),
	file_unique_id := binary(),
	file_size := integer(),
	file_date := integer()
}.

-doc """
Describes documents or other Telegram Passport elements shared with the bot by the user.
  * `type` - Element type. One of “personal_details”, “passport”, “driver_license”, “identity_card”, “internal_passport”, “address”, “utility_bill”, “bank_statement”, “rental_agreement”, “passport_registration”, “temporary_registration”, “phone_number”, “email”.
  * `data` - Optional. Base64-encoded encrypted Telegram Passport element data provided by the user; available only for “personal_details”, “passport”, “driver_license”, “identity_card”, “internal_passport” and “address” types. Can be decrypted and verified using the accompanying EncryptedCredentials.
  * `phone_number` - Optional. User's verified phone number; available only for “phone_number” type
  * `email` - Optional. User's verified email address; available only for “email” type
  * `files` - Optional. Array of encrypted files with documents provided by the user; available only for “utility_bill”, “bank_statement”, “rental_agreement”, “passport_registration” and “temporary_registration” types. Files can be decrypted and verified using the accompanying EncryptedCredentials.
  * `front_side` - Optional. Encrypted file with the front side of the document, provided by the user; available only for “passport”, “driver_license”, “identity_card” and “internal_passport”. The file can be decrypted and verified using the accompanying EncryptedCredentials.
  * `reverse_side` - Optional. Encrypted file with the reverse side of the document, provided by the user; available only for “driver_license” and “identity_card”. The file can be decrypted and verified using the accompanying EncryptedCredentials.
  * `selfie` - Optional. Encrypted file with the selfie of the user holding a document, provided by the user; available if requested for “passport”, “driver_license”, “identity_card” and “internal_passport”. The file can be decrypted and verified using the accompanying EncryptedCredentials.
  * `translation` - Optional. Array of encrypted files with translated versions of documents provided by the user; available if requested for “passport”, “driver_license”, “identity_card”, “internal_passport”, “utility_bill”, “bank_statement”, “rental_agreement”, “passport_registration” and “temporary_registration” types. Files can be decrypted and verified using the accompanying EncryptedCredentials.
  * `hash` - Base64-encoded element hash for using in PassportElementErrorUnspecified
""".
-type 'EncryptedPassportElement'() :: #{
	type := binary(),
	data => binary(),
	phone_number => binary(),
	email => binary(),
	files => nonempty_list('PassportFile'()),
	front_side => 'PassportFile'(),
	reverse_side => 'PassportFile'(),
	selfie => 'PassportFile'(),
	translation => nonempty_list('PassportFile'()),
	hash := binary()
}.

-doc """
Describes data required for decrypting and authenticating EncryptedPassportElement.  
See the Telegram Passport Documentation for a complete description of the data decryption and authentication processes.
  * `data` - Base64-encoded encrypted JSON-serialized data with unique user's payload, data hashes and secrets required for EncryptedPassportElement decryption and authentication
  * `hash` - Base64-encoded data hash for data authentication
  * `secret` - Base64-encoded secret, encrypted with the bot's public RSA key, required for data decryption
""".
-type 'EncryptedCredentials'() :: #{
	data := binary(),
	hash := binary(),
	secret := binary()
}.

-doc """
This object represents an error in the Telegram Passport element which was submitted that should be resolved by the user.  
It should be one of:
""".
-type 'PassportElementError'() :: 'PassportElementErrorDataField'() | 'PassportElementErrorFrontSide'() | 'PassportElementErrorReverseSide'() | 'PassportElementErrorSelfie'() | 'PassportElementErrorFile'() | 'PassportElementErrorFiles'() | 'PassportElementErrorTranslationFile'() | 'PassportElementErrorTranslationFiles'() | 'PassportElementErrorUnspecified'().

-doc """
Represents an issue in one of the data fields that was provided by the user.  
The error is considered resolved when the field's value changes.
  * `source` - Error source, must be data
  * `type` - The section of the user's Telegram Passport which has the error, one of “personal_details”, “passport”, “driver_license”, “identity_card”, “internal_passport”, “address”
  * `field_name` - Name of the data field which has the error
  * `data_hash` - Base64-encoded data hash
  * `message` - Error message
""".
-type 'PassportElementErrorDataField'() :: #{
	source := binary(),
	type := binary(),
	field_name := binary(),
	data_hash := binary(),
	message := binary()
}.

-doc """
Represents an issue with the front side of a document.  
The error is considered resolved when the file with the front side of the document changes.
  * `source` - Error source, must be front_side
  * `type` - The section of the user's Telegram Passport which has the issue, one of “passport”, “driver_license”, “identity_card”, “internal_passport”
  * `file_hash` - Base64-encoded hash of the file with the front side of the document
  * `message` - Error message
""".
-type 'PassportElementErrorFrontSide'() :: #{
	source := binary(),
	type := binary(),
	file_hash := binary(),
	message := binary()
}.

-doc """
Represents an issue with the reverse side of a document.  
The error is considered resolved when the file with reverse side of the document changes.
  * `source` - Error source, must be reverse_side
  * `type` - The section of the user's Telegram Passport which has the issue, one of “driver_license”, “identity_card”
  * `file_hash` - Base64-encoded hash of the file with the reverse side of the document
  * `message` - Error message
""".
-type 'PassportElementErrorReverseSide'() :: #{
	source := binary(),
	type := binary(),
	file_hash := binary(),
	message := binary()
}.

-doc """
Represents an issue with the selfie with a document.  
The error is considered resolved when the file with the selfie changes.
  * `source` - Error source, must be selfie
  * `type` - The section of the user's Telegram Passport which has the issue, one of “passport”, “driver_license”, “identity_card”, “internal_passport”
  * `file_hash` - Base64-encoded hash of the file with the selfie
  * `message` - Error message
""".
-type 'PassportElementErrorSelfie'() :: #{
	source := binary(),
	type := binary(),
	file_hash := binary(),
	message := binary()
}.

-doc """
Represents an issue with a document scan.  
The error is considered resolved when the file with the document scan changes.
  * `source` - Error source, must be file
  * `type` - The section of the user's Telegram Passport which has the issue, one of “utility_bill”, “bank_statement”, “rental_agreement”, “passport_registration”, “temporary_registration”
  * `file_hash` - Base64-encoded file hash
  * `message` - Error message
""".
-type 'PassportElementErrorFile'() :: #{
	source := binary(),
	type := binary(),
	file_hash := binary(),
	message := binary()
}.

-doc """
Represents an issue with a list of scans.  
The error is considered resolved when the list of files containing the scans changes.
  * `source` - Error source, must be files
  * `type` - The section of the user's Telegram Passport which has the issue, one of “utility_bill”, “bank_statement”, “rental_agreement”, “passport_registration”, “temporary_registration”
  * `file_hashes` - List of base64-encoded file hashes
  * `message` - Error message
""".
-type 'PassportElementErrorFiles'() :: #{
	source := binary(),
	type := binary(),
	file_hashes := nonempty_list(binary()),
	message := binary()
}.

-doc """
Represents an issue with one of the files that constitute the translation of a document.  
The error is considered resolved when the file changes.
  * `source` - Error source, must be translation_file
  * `type` - Type of element of the user's Telegram Passport which has the issue, one of “passport”, “driver_license”, “identity_card”, “internal_passport”, “utility_bill”, “bank_statement”, “rental_agreement”, “passport_registration”, “temporary_registration”
  * `file_hash` - Base64-encoded file hash
  * `message` - Error message
""".
-type 'PassportElementErrorTranslationFile'() :: #{
	source := binary(),
	type := binary(),
	file_hash := binary(),
	message := binary()
}.

-doc """
Represents an issue with the translated version of a document.  
The error is considered resolved when a file with the document translation change.
  * `source` - Error source, must be translation_files
  * `type` - Type of element of the user's Telegram Passport which has the issue, one of “passport”, “driver_license”, “identity_card”, “internal_passport”, “utility_bill”, “bank_statement”, “rental_agreement”, “passport_registration”, “temporary_registration”
  * `file_hashes` - List of base64-encoded file hashes
  * `message` - Error message
""".
-type 'PassportElementErrorTranslationFiles'() :: #{
	source := binary(),
	type := binary(),
	file_hashes := nonempty_list(binary()),
	message := binary()
}.

-doc """
Represents an issue in an unspecified place.  
The error is considered resolved when new data is added.
  * `source` - Error source, must be unspecified
  * `type` - Type of element of the user's Telegram Passport which has the issue
  * `element_hash` - Base64-encoded element hash
  * `message` - Error message
""".
-type 'PassportElementErrorUnspecified'() :: #{
	source := binary(),
	type := binary(),
	element_hash := binary(),
	message := binary()
}.

-doc """
This object represents a game.  
Use BotFather to create and edit games, their short names will act as unique identifiers.
  * `title` - Title of the game
  * `description` - Description of the game
  * `photo` - Photo that will be displayed in the game message in chats.
  * `text` - Optional. Brief description of the game or high scores included in the game message. Can be automatically edited to include current high scores for the game when the bot calls setGameScore, or manually edited using editMessageText. 0-4096 characters.
  * `text_entities` - Optional. Special entities that appear in text, such as usernames, URLs, bot commands, etc.
  * `animation` - Optional. Animation that will be displayed in the game message in chats. Upload via BotFather
""".
-type 'Game'() :: #{
	title := binary(),
	description := binary(),
	photo := nonempty_list('PhotoSize'()),
	text => binary(),
	text_entities => nonempty_list('MessageEntity'()),
	animation => 'Animation'()
}.

-doc """
A placeholder, currently holds no information.  
Use BotFather to set up your game.
""".
-type 'CallbackGame'() :: empty_map().

-doc """
This object represents one row of the high scores table for a game.
  * `position` - Position in high score table for the game
  * `user` - User
  * `score` - Score
""".
-type 'GameHighScore'() :: #{
	position := integer(),
	user := 'User'(),
	score := integer()
}.

%%==========================================



-doc """
Use this method to receive incoming updates using long polling (wiki).  
Returns an Array of Update objects.  
Notes
1.  
This method will not work if an outgoing webhook is set up.  
2.  
In order to avoid getting duplicate updates, recalculate offset after each server response.
## Parameters
  * `offset` - Identifier of the first update to be returned. Must be greater by one than the highest among the identifiers of previously received updates. By default, updates starting with the earliest unconfirmed update are returned. An update is considered confirmed as soon as getUpdates is called with an offset higher than its update_id. The negative offset can be specified to retrieve updates starting from -offset update from the end of the updates queue. All previous updates will be forgotten.
  * `limit` - Limits the number of updates to be retrieved. Values between 1-100 are accepted. Defaults to 100.
  * `timeout` - Timeout in seconds for long polling. Defaults to 0, i.e. usual short polling. Should be positive, short polling should be used for testing purposes only.
  * `allowed_updates` - A JSON-serialized list of the update types you want your bot to receive. For example, specify [message, edited_channel_post, callback_query] to only receive updates of these types. See Update for a complete list of available update types. Specify an empty list to receive all update types except chat_member, message_reaction, and message_reaction_count (default). If not specified, the previous setting will be used.

Please note that this parameter doesn't affect updates created before the call to getUpdates, so unwanted updates may be received for a short period of time.
""".
-doc (#{group=><<"Long Polling">>,since=><<"2.3">>}).
-spec getUpdates(Pool :: pool_name(), Req :: #{offset => integer(), limit => integer(), timeout => integer(), allowed_updates => nonempty_list('update_type'())}, Async :: boolean()) -> Result :: result(nonempty_list('Update'())).
getUpdates(Pool, #{} = Req, Async) ->wpool:call(Pool, {raw, <<"getUpdates">>, Req, Async}).
-doc (#{equiv=>getUpdates(Pool, Req, false),since=><<"2.3">>,group=><<"Sync Request">>}).
getUpdates(Pool, Req) -> getUpdates(Pool, Req, false).


-doc """
Use this method to specify a URL and receive incoming updates via an outgoing webhook.  
Whenever there is an update for the bot, we will send an HTTPS POST request to the specified URL, containing a JSON-serialized Update.  
In case of an unsuccessful request (a request with response HTTP status code different from 2XY), we will repeat the request and give up after a reasonable amount of attempts.  
Returns True on success.  
If you'd like to make sure that the webhook was set by you, you can specify secret data in the parameter secret_token.  
If specified, the request will contain a header “X-Telegram-Bot-Api-Secret-Token” with the secret token as content.  
Notes
1.  
You will not be able to receive updates using getUpdates for as long as an outgoing webhook is set up.  
2.  
To use a self-signed certificate, you need to upload your public key certificate using certificate parameter.  
Please upload as InputFile, sending a String will not work.  
3.  
Ports currently supported for webhooks: 443, 80, 88, 8443.  
If you're having any trouble setting up webhooks, please check out this amazing guide to webhooks.
## Parameters
  * `url` - HTTPS URL to send updates to. Use an empty string to remove webhook integration
  * `certificate` - Upload your public key certificate so that the root certificate in use can be checked. See our self-signed guide for details.
  * `ip_address` - The fixed IP address which will be used to send webhook requests instead of the IP address resolved through DNS
  * `max_connections` - The maximum allowed number of simultaneous HTTPS connections to the webhook for update delivery, 1-100. Defaults to 40. Use lower values to limit the load on your bot's server, and higher values to increase your bot's throughput.
  * `allowed_updates` - A JSON-serialized list of the update types you want your bot to receive. For example, specify [message, edited_channel_post, callback_query] to only receive updates of these types. See Update for a complete list of available update types. Specify an empty list to receive all update types except chat_member, message_reaction, and message_reaction_count (default). If not specified, the previous setting will be used.
Please note that this parameter doesn't affect updates created before the call to the setWebhook, so unwanted updates may be received for a short period of time.
  * `drop_pending_updates` - Pass True to drop all pending updates
  * `secret_token` - A secret token to be sent in a header “X-Telegram-Bot-Api-Secret-Token” in every webhook request, 1-256 characters. Only characters A-Z, a-z, 0-9, _ and - are allowed. The header is useful to ensure that the request comes from a webhook set by you.
""".
-doc (#{group=><<"">>,since=><<"1.15">>}).
-spec setWebhook(Pool :: pool_name(), Req :: #{url := binary(), certificate => 'InputFile'(), ip_address => binary(), max_connections => integer(), allowed_updates => nonempty_list('update_type'()), drop_pending_updates => boolean(), secret_token => secret_token()}, Async :: boolean()) -> Result :: result(true).
setWebhook(Pool, #{url:=_} = Req, Async) ->wpool:call(Pool, {multipart, <<"setWebhook">>, Req, Async}).
-doc (#{equiv=>setWebhook(Pool, Req, false),since=><<"1.15">>,group=><<"Sync Request">>}).
setWebhook(Pool, Req) -> setWebhook(Pool, Req, false).


-doc """
Use this method to remove webhook integration if you decide to switch back to getUpdates.  
Returns True on success.
## Parameters
  * `drop_pending_updates` - Pass True to drop all pending updates
""".
-doc (#{group=><<"">>,since=><<"2.3">>}).
-spec deleteWebhook(Pool :: pool_name(), Req :: #{drop_pending_updates => boolean()}, Async :: boolean()) -> Result :: result(true).
deleteWebhook(Pool, #{} = Req, Async) ->wpool:call(Pool, {raw, <<"deleteWebhook">>, Req, Async}).
-doc (#{equiv=>deleteWebhook(Pool, Req, false),since=><<"2.3">>,group=><<"Sync Request">>}).
deleteWebhook(Pool, Req) -> deleteWebhook(Pool, Req, false).


-doc """
Use this method to get current webhook status.  
Requires no parameters.  
On success, returns a WebhookInfo object.  
If the bot is using getUpdates, will return an object with the url field empty.
""".
-doc (#{group=><<"">>,since=><<"2.2">>}).
-spec getWebhookInfo(Pool :: pool_name(), Req :: empty_map(), Async :: boolean()) -> Result :: result('WebhookInfo'()).
getWebhookInfo(Pool, #{} = Req, Async) ->wpool:call(Pool, {raw, <<"getWebhookInfo">>, Req, Async}).
-doc (#{equiv=>getWebhookInfo(Pool, Req, false),since=><<"2.2">>,group=><<"Sync Request">>}).
getWebhookInfo(Pool, Req) -> getWebhookInfo(Pool, Req, false).


-doc """
A simple method for testing your bot's authentication token.  
Requires no parameters.  
Returns basic information about the bot in form of a User object.
""".
-doc (#{group=><<"Bot Settings">>,since=><<"4.6">>}).
-spec getMe(Pool :: pool_name(), Req :: empty_map(), Async :: boolean()) -> Result :: result('User'()).
getMe(Pool, #{} = Req, Async) ->wpool:call(Pool, {raw, <<"getMe">>, Req, Async}).
-doc (#{equiv=>getMe(Pool, Req, false),since=><<"4.6">>,group=><<"Sync Request">>}).
getMe(Pool, Req) -> getMe(Pool, Req, false).


-doc """
Use this method to log out from the cloud Bot API server before launching the bot locally.  
You must log out the bot before running it locally, otherwise there is no guarantee that the bot will receive updates.  
After a successful call, you can immediately log in on a local server, but will not be able to log in back to the cloud Bot API server for 10 minutes.  
Returns True on success.  
Requires no parameters.
""".
-doc (#{group=><<"">>,since=><<"5.0">>}).
-spec logOut(Pool :: pool_name(), Req :: empty_map(), Async :: boolean()) -> Result :: result(true).
logOut(Pool, #{} = Req, Async) ->wpool:call(Pool, {raw, <<"logOut">>, Req, Async}).
-doc (#{equiv=>logOut(Pool, Req, false),since=><<"5.0">>,group=><<"Sync Request">>}).
logOut(Pool, Req) -> logOut(Pool, Req, false).


-doc """
Use this method to close the bot instance before moving it from one local server to another.  
You need to delete the webhook before calling this method to ensure that the bot isn't launched again after server restart.  
The method will return error 429 in the first 10 minutes after the bot is launched.  
Returns True on success.  
Requires no parameters.  
[See moving a bot from one local server](https://github.com/tdlib/telegram-bot-api/tree/master?tab=readme-ov-file#moving-a-bot-from-one-local-server-to-another)
""".
-doc (#{group=><<"Local Server">>,since=><<"4.6">>}).
-spec close(Pool :: pool_name(), Req :: empty_map(), Async :: boolean()) -> Result :: result(true).
close(Pool, #{} = Req, Async) ->wpool:call(Pool, {raw, <<"close">>, Req, Async}).
-doc (#{equiv=>close(Pool, Req, false),since=><<"4.6">>,group=><<"Sync Request">>}).
close(Pool, Req) -> close(Pool, Req, false).


-doc """
Use this method to send text messages.  
On success, the sent Message is returned.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the message will be sent
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_thread_id` - Unique identifier for the target message thread (topic) of a forum; for forum supergroups and private chats of bots with forum topic mode enabled only
  * `direct_messages_topic_id` - Identifier of the direct messages topic to which the message will be sent; required if the message is sent to a direct messages chat
  * `text` - Text of the message to be sent, 1-4096 characters after entities parsing
  * `parse_mode` - Mode for parsing entities in the message text. See formatting options for more details.
  * `entities` - A JSON-serialized list of special entities that appear in message text, which can be specified instead of parse_mode
  * `link_preview_options` - Link preview generation options for the message
  * `disable_notification` - Sends the message silently. Users will receive a notification with no sound.
  * `protect_content` - Protects the contents of the sent message from forwarding and saving
  * `allow_paid_broadcast` - Pass True to allow up to 1000 messages per second, ignoring broadcasting limits for a fee of 0.1 Telegram Stars per message. The relevant Stars will be withdrawn from the bot's balance
  * `message_effect_id` - Unique identifier of the message effect to be added to the message; for private chats only
  * `suggested_post_parameters` - A JSON-serialized object containing the parameters of the suggested post to send; for direct messages chats only. If the message is sent as a reply to another suggested post, then that suggested post is automatically declined.
  * `reply_parameters` - Description of the message to reply to
  * `reply_markup` - Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove a reply keyboard or to force a reply from the user
""".
-doc (#{group=><<"Message">>,since=><<"1.15">>}).
-spec sendMessage(Pool :: pool_name(), Req :: #{business_connection_id => binary(), chat_id := integer() | binary(), message_thread_id => integer(), direct_messages_topic_id => integer(), text := binary(), parse_mode => binary(), entities => nonempty_list('MessageEntity'()), link_preview_options => 'LinkPreviewOptions'(), disable_notification => boolean(), protect_content => boolean(), allow_paid_broadcast => boolean(), message_effect_id => binary(), suggested_post_parameters => 'SuggestedPostParameters'(), reply_parameters => 'ReplyParameters'(), reply_markup => 'InlineKeyboardMarkup'() | 'ReplyKeyboardMarkup'() | 'ReplyKeyboardRemove'() | 'ForceReply'()}, Async :: boolean()) -> Result :: result('Message'()).
sendMessage(Pool, #{chat_id:=_,text:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"sendMessage">>, Req, Async}).
-doc (#{equiv=>sendMessage(Pool, Req, false),since=><<"1.15">>,group=><<"Sync Request">>}).
sendMessage(Pool, Req) -> sendMessage(Pool, Req, false).


-doc """
Use this method to forward messages of any kind.  
Service messages and messages with protected content can't be forwarded.  
On success, the sent Message is returned.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_thread_id` - Unique identifier for the target message thread (topic) of a forum; for forum supergroups and private chats of bots with forum topic mode enabled only
  * `direct_messages_topic_id` - Identifier of the direct messages topic to which the message will be forwarded; required if the message is forwarded to a direct messages chat
  * `from_chat_id` - Unique identifier for the chat where the original message was sent (or channel username in the format @channelusername)
  * `video_start_timestamp` - New start timestamp for the forwarded video in the message
  * `disable_notification` - Sends the message silently. Users will receive a notification with no sound.
  * `protect_content` - Protects the contents of the forwarded message from forwarding and saving
  * `message_effect_id` - Unique identifier of the message effect to be added to the message; only available when forwarding to private chats
  * `suggested_post_parameters` - A JSON-serialized object containing the parameters of the suggested post to send; for direct messages chats only
  * `message_id` - Message identifier in the chat specified in from_chat_id
""".
-doc (#{group=><<"Message">>,since=><<"1.15">>}).
-spec forwardMessage(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), message_thread_id => integer(), direct_messages_topic_id => integer(), from_chat_id := integer() | binary(), video_start_timestamp => integer(), disable_notification => boolean(), protect_content => boolean(), message_effect_id => binary(), suggested_post_parameters => 'SuggestedPostParameters'(), message_id := integer()}, Async :: boolean()) -> Result :: result('Message'()).
forwardMessage(Pool, #{chat_id:=_,from_chat_id:=_,message_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"forwardMessage">>, Req, Async}).
-doc (#{equiv=>forwardMessage(Pool, Req, false),since=><<"1.15">>,group=><<"Sync Request">>}).
forwardMessage(Pool, Req) -> forwardMessage(Pool, Req, false).


-doc """
Use this method to forward multiple messages of any kind.  
If some of the specified messages can't be found or forwarded, they are skipped.  
Service messages and messages with protected content can't be forwarded.  
Album grouping is kept for forwarded messages.  
On success, an array of MessageId of the sent messages is returned.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_thread_id` - Unique identifier for the target message thread (topic) of a forum; for forum supergroups and private chats of bots with forum topic mode enabled only
  * `direct_messages_topic_id` - Identifier of the direct messages topic to which the messages will be forwarded; required if the messages are forwarded to a direct messages chat
  * `from_chat_id` - Unique identifier for the chat where the original messages were sent (or channel username in the format @channelusername)
  * `message_ids` - A JSON-serialized list of 1-100 identifiers of messages in the chat from_chat_id to forward. The identifiers must be specified in a strictly increasing order.
  * `disable_notification` - Sends the messages silently. Users will receive a notification with no sound.
  * `protect_content` - Protects the contents of the forwarded messages from forwarding and saving
""".
-doc (#{group=><<"Message">>,since=><<"7.0">>}).
-spec forwardMessages(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), message_thread_id => integer(), direct_messages_topic_id => integer(), from_chat_id := integer() | binary(), message_ids := nonempty_list(integer()), disable_notification => boolean(), protect_content => boolean()}, Async :: boolean()) -> Result :: result(nonempty_list('MessageId'())).
forwardMessages(Pool, #{chat_id:=_,from_chat_id:=_,message_ids:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"forwardMessages">>, Req, Async}).
-doc (#{equiv=>forwardMessages(Pool, Req, false),since=><<"7.0">>,group=><<"Sync Request">>}).
forwardMessages(Pool, Req) -> forwardMessages(Pool, Req, false).


-doc """
Use this method to copy messages of any kind.  
Service messages, paid media messages, giveaway messages, giveaway winners messages, and invoice messages can't be copied.  
A quiz poll can be copied only if the value of the field correct_option_id is known to the bot.  
The method is analogous to the method forwardMessage, but the copied message doesn't have a link to the original message.  
Returns the MessageId of the sent message on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_thread_id` - Unique identifier for the target message thread (topic) of a forum; for forum supergroups and private chats of bots with forum topic mode enabled only
  * `direct_messages_topic_id` - Identifier of the direct messages topic to which the message will be sent; required if the message is sent to a direct messages chat
  * `from_chat_id` - Unique identifier for the chat where the original message was sent (or channel username in the format @channelusername)
  * `message_id` - Message identifier in the chat specified in from_chat_id
  * `video_start_timestamp` - New start timestamp for the copied video in the message
  * `caption` - New caption for media, 0-1024 characters after entities parsing. If not specified, the original caption is kept
  * `parse_mode` - Mode for parsing entities in the new caption. See formatting options for more details.
  * `caption_entities` - A JSON-serialized list of special entities that appear in the new caption, which can be specified instead of parse_mode
  * `show_caption_above_media` - Pass True, if the caption must be shown above the message media. Ignored if a new caption isn't specified.
  * `disable_notification` - Sends the message silently. Users will receive a notification with no sound.
  * `protect_content` - Protects the contents of the sent message from forwarding and saving
  * `allow_paid_broadcast` - Pass True to allow up to 1000 messages per second, ignoring broadcasting limits for a fee of 0.1 Telegram Stars per message. The relevant Stars will be withdrawn from the bot's balance
  * `message_effect_id` - Unique identifier of the message effect to be added to the message; only available when copying to private chats
  * `suggested_post_parameters` - A JSON-serialized object containing the parameters of the suggested post to send; for direct messages chats only. If the message is sent as a reply to another suggested post, then that suggested post is automatically declined.
  * `reply_parameters` - Description of the message to reply to
  * `reply_markup` - Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove a reply keyboard or to force a reply from the user
""".
-doc (#{group=><<"Message">>,since=><<"5.0">>}).
-spec copyMessage(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), message_thread_id => integer(), direct_messages_topic_id => integer(), from_chat_id := integer() | binary(), message_id := integer(), video_start_timestamp => integer(), caption => binary(), parse_mode => binary(), caption_entities => nonempty_list('MessageEntity'()), show_caption_above_media => boolean(), disable_notification => boolean(), protect_content => boolean(), allow_paid_broadcast => boolean(), message_effect_id => binary(), suggested_post_parameters => 'SuggestedPostParameters'(), reply_parameters => 'ReplyParameters'(), reply_markup => 'InlineKeyboardMarkup'() | 'ReplyKeyboardMarkup'() | 'ReplyKeyboardRemove'() | 'ForceReply'()}, Async :: boolean()) -> Result :: result('MessageId'()).
copyMessage(Pool, #{chat_id:=_,from_chat_id:=_,message_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"copyMessage">>, Req, Async}).
-doc (#{equiv=>copyMessage(Pool, Req, false),since=><<"5.0">>,group=><<"Sync Request">>}).
copyMessage(Pool, Req) -> copyMessage(Pool, Req, false).


-doc """
Use this method to copy messages of any kind.  
If some of the specified messages can't be found or copied, they are skipped.  
Service messages, paid media messages, giveaway messages, giveaway winners messages, and invoice messages can't be copied.  
A quiz poll can be copied only if the value of the field correct_option_id is known to the bot.  
The method is analogous to the method forwardMessages, but the copied messages don't have a link to the original message.  
Album grouping is kept for copied messages.  
On success, an array of MessageId of the sent messages is returned.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_thread_id` - Unique identifier for the target message thread (topic) of a forum; for forum supergroups and private chats of bots with forum topic mode enabled only
  * `direct_messages_topic_id` - Identifier of the direct messages topic to which the messages will be sent; required if the messages are sent to a direct messages chat
  * `from_chat_id` - Unique identifier for the chat where the original messages were sent (or channel username in the format @channelusername)
  * `message_ids` - A JSON-serialized list of 1-100 identifiers of messages in the chat from_chat_id to copy. The identifiers must be specified in a strictly increasing order.
  * `disable_notification` - Sends the messages silently. Users will receive a notification with no sound.
  * `protect_content` - Protects the contents of the sent messages from forwarding and saving
  * `remove_caption` - Pass True to copy the messages without their captions
""".
-doc (#{group=><<"Message">>,since=><<"7.0">>}).
-spec copyMessages(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), message_thread_id => integer(), direct_messages_topic_id => integer(), from_chat_id := integer() | binary(), message_ids := nonempty_list(integer()), disable_notification => boolean(), protect_content => boolean(), remove_caption => boolean()}, Async :: boolean()) -> Result :: result(nonempty_list('MessageId'())).
copyMessages(Pool, #{chat_id:=_,from_chat_id:=_,message_ids:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"copyMessages">>, Req, Async}).
-doc (#{equiv=>copyMessages(Pool, Req, false),since=><<"7.0">>,group=><<"Sync Request">>}).
copyMessages(Pool, Req) -> copyMessages(Pool, Req, false).


-doc """
Use this method to send photos.  
On success, the sent Message is returned.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the message will be sent
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_thread_id` - Unique identifier for the target message thread (topic) of a forum; for forum supergroups and private chats of bots with forum topic mode enabled only
  * `direct_messages_topic_id` - Identifier of the direct messages topic to which the message will be sent; required if the message is sent to a direct messages chat
  * `photo` - Photo to send. Pass a file_id as String to send a photo that exists on the Telegram servers (recommended), pass an HTTP URL as a String for Telegram to get a photo from the Internet, or upload a new photo using multipart/form-data. The photo must be at most 10 MB in size. The photo's width and height must not exceed 10000 in total. Width and height ratio must be at most 20. More information on Sending Files »
  * `caption` - Photo caption (may also be used when resending photos by file_id), 0-1024 characters after entities parsing
  * `parse_mode` - Mode for parsing entities in the photo caption. See formatting options for more details.
  * `caption_entities` - A JSON-serialized list of special entities that appear in the caption, which can be specified instead of parse_mode
  * `show_caption_above_media` - Pass True, if the caption must be shown above the message media
  * `has_spoiler` - Pass True if the photo needs to be covered with a spoiler animation
  * `disable_notification` - Sends the message silently. Users will receive a notification with no sound.
  * `protect_content` - Protects the contents of the sent message from forwarding and saving
  * `allow_paid_broadcast` - Pass True to allow up to 1000 messages per second, ignoring broadcasting limits for a fee of 0.1 Telegram Stars per message. The relevant Stars will be withdrawn from the bot's balance
  * `message_effect_id` - Unique identifier of the message effect to be added to the message; for private chats only
  * `suggested_post_parameters` - A JSON-serialized object containing the parameters of the suggested post to send; for direct messages chats only. If the message is sent as a reply to another suggested post, then that suggested post is automatically declined.
  * `reply_parameters` - Description of the message to reply to
  * `reply_markup` - Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove a reply keyboard or to force a reply from the user
""".
-doc (#{group=><<"Message">>,since=><<"5.0">>}).
-spec sendPhoto(Pool :: pool_name(), Req :: #{business_connection_id => binary(), chat_id := integer() | binary(), message_thread_id => integer(), direct_messages_topic_id => integer(), photo := 'InputFile'() | binary(), caption => binary(), parse_mode => binary(), caption_entities => nonempty_list('MessageEntity'()), show_caption_above_media => boolean(), has_spoiler => boolean(), disable_notification => boolean(), protect_content => boolean(), allow_paid_broadcast => boolean(), message_effect_id => binary(), suggested_post_parameters => 'SuggestedPostParameters'(), reply_parameters => 'ReplyParameters'(), reply_markup => 'InlineKeyboardMarkup'() | 'ReplyKeyboardMarkup'() | 'ReplyKeyboardRemove'() | 'ForceReply'()}, Async :: boolean()) -> Result :: result('Message'()).
sendPhoto(Pool, #{chat_id:=_,photo:=_} = Req, Async) ->wpool:call(Pool, {multipart, <<"sendPhoto">>, Req, Async}).
-doc (#{equiv=>sendPhoto(Pool, Req, false),since=><<"5.0">>,group=><<"Sync Request">>}).
sendPhoto(Pool, Req) -> sendPhoto(Pool, Req, false).


-doc """
Use this method to send audio files, if you want Telegram clients to display them in the music player.  
Your audio must be in the .MP3 or .M4A format.  
On success, the sent Message is returned.  
Bots can currently send audio files of up to 50 MB in size, this limit may be changed in the future.  
For sending voice messages, use the sendVoice method instead.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the message will be sent
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_thread_id` - Unique identifier for the target message thread (topic) of a forum; for forum supergroups and private chats of bots with forum topic mode enabled only
  * `direct_messages_topic_id` - Identifier of the direct messages topic to which the message will be sent; required if the message is sent to a direct messages chat
  * `audio` - Audio file to send. Pass a file_id as String to send an audio file that exists on the Telegram servers (recommended), pass an HTTP URL as a String for Telegram to get an audio file from the Internet, or upload a new one using multipart/form-data. More information on Sending Files »
  * `caption` - Audio caption, 0-1024 characters after entities parsing
  * `parse_mode` - Mode for parsing entities in the audio caption. See formatting options for more details.
  * `caption_entities` - A JSON-serialized list of special entities that appear in the caption, which can be specified instead of parse_mode
  * `duration` - Duration of the audio in seconds
  * `performer` - Performer
  * `title` - Track name
  * `thumbnail` - Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://<file_attach_name>” if the thumbnail was uploaded using multipart/form-data under <file_attach_name>. More information on Sending Files »
  * `disable_notification` - Sends the message silently. Users will receive a notification with no sound.
  * `protect_content` - Protects the contents of the sent message from forwarding and saving
  * `allow_paid_broadcast` - Pass True to allow up to 1000 messages per second, ignoring broadcasting limits for a fee of 0.1 Telegram Stars per message. The relevant Stars will be withdrawn from the bot's balance
  * `message_effect_id` - Unique identifier of the message effect to be added to the message; for private chats only
  * `suggested_post_parameters` - A JSON-serialized object containing the parameters of the suggested post to send; for direct messages chats only. If the message is sent as a reply to another suggested post, then that suggested post is automatically declined.
  * `reply_parameters` - Description of the message to reply to
  * `reply_markup` - Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove a reply keyboard or to force a reply from the user
""".
-doc (#{group=><<"Message">>,since=><<"1.15">>}).
-spec sendAudio(Pool :: pool_name(), Req :: #{business_connection_id => binary(), chat_id := integer() | binary(), message_thread_id => integer(), direct_messages_topic_id => integer(), audio := 'InputFile'() | binary(), caption => binary(), parse_mode => binary(), caption_entities => nonempty_list('MessageEntity'()), duration => integer(), performer => binary(), title => binary(), thumbnail => 'InputFile'() | binary(), disable_notification => boolean(), protect_content => boolean(), allow_paid_broadcast => boolean(), message_effect_id => binary(), suggested_post_parameters => 'SuggestedPostParameters'(), reply_parameters => 'ReplyParameters'(), reply_markup => 'InlineKeyboardMarkup'() | 'ReplyKeyboardMarkup'() | 'ReplyKeyboardRemove'() | 'ForceReply'()}, Async :: boolean()) -> Result :: result('Message'()).
sendAudio(Pool, #{chat_id:=_,audio:=_} = Req, Async) ->wpool:call(Pool, {multipart, <<"sendAudio">>, Req, Async}).
-doc (#{equiv=>sendAudio(Pool, Req, false),since=><<"1.15">>,group=><<"Sync Request">>}).
sendAudio(Pool, Req) -> sendAudio(Pool, Req, false).


-doc """
Use this method to send general files.  
On success, the sent Message is returned.  
Bots can currently send files of any type of up to 50 MB in size, this limit may be changed in the future.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the message will be sent
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_thread_id` - Unique identifier for the target message thread (topic) of a forum; for forum supergroups and private chats of bots with forum topic mode enabled only
  * `direct_messages_topic_id` - Identifier of the direct messages topic to which the message will be sent; required if the message is sent to a direct messages chat
  * `document` - File to send. Pass a file_id as String to send a file that exists on the Telegram servers (recommended), pass an HTTP URL as a String for Telegram to get a file from the Internet, or upload a new one using multipart/form-data. More information on Sending Files »
  * `thumbnail` - Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://<file_attach_name>” if the thumbnail was uploaded using multipart/form-data under <file_attach_name>. More information on Sending Files »
  * `caption` - Document caption (may also be used when resending documents by file_id), 0-1024 characters after entities parsing
  * `parse_mode` - Mode for parsing entities in the document caption. See formatting options for more details.
  * `caption_entities` - A JSON-serialized list of special entities that appear in the caption, which can be specified instead of parse_mode
  * `disable_content_type_detection` - Disables automatic server-side content type detection for files uploaded using multipart/form-data
  * `disable_notification` - Sends the message silently. Users will receive a notification with no sound.
  * `protect_content` - Protects the contents of the sent message from forwarding and saving
  * `allow_paid_broadcast` - Pass True to allow up to 1000 messages per second, ignoring broadcasting limits for a fee of 0.1 Telegram Stars per message. The relevant Stars will be withdrawn from the bot's balance
  * `message_effect_id` - Unique identifier of the message effect to be added to the message; for private chats only
  * `suggested_post_parameters` - A JSON-serialized object containing the parameters of the suggested post to send; for direct messages chats only. If the message is sent as a reply to another suggested post, then that suggested post is automatically declined.
  * `reply_parameters` - Description of the message to reply to
  * `reply_markup` - Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove a reply keyboard or to force a reply from the user
""".
-doc (#{group=><<"Message">>,since=><<"4.0">>}).
-spec sendDocument(Pool :: pool_name(), Req :: #{business_connection_id => binary(), chat_id := integer() | binary(), message_thread_id => integer(), direct_messages_topic_id => integer(), document := 'InputFile'() | binary(), thumbnail => 'InputFile'() | binary(), caption => binary(), parse_mode => binary(), caption_entities => nonempty_list('MessageEntity'()), disable_content_type_detection => boolean(), disable_notification => boolean(), protect_content => boolean(), allow_paid_broadcast => boolean(), message_effect_id => binary(), suggested_post_parameters => 'SuggestedPostParameters'(), reply_parameters => 'ReplyParameters'(), reply_markup => 'InlineKeyboardMarkup'() | 'ReplyKeyboardMarkup'() | 'ReplyKeyboardRemove'() | 'ForceReply'()}, Async :: boolean()) -> Result :: result('Message'()).
sendDocument(Pool, #{chat_id:=_,document:=_} = Req, Async) ->wpool:call(Pool, {multipart, <<"sendDocument">>, Req, Async}).
-doc (#{equiv=>sendDocument(Pool, Req, false),since=><<"4.0">>,group=><<"Sync Request">>}).
sendDocument(Pool, Req) -> sendDocument(Pool, Req, false).


-doc """
Use this method to send video files, Telegram clients support MPEG4 videos (other formats may be sent as Document).  
On success, the sent Message is returned.  
Bots can currently send video files of up to 50 MB in size, this limit may be changed in the future.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the message will be sent
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_thread_id` - Unique identifier for the target message thread (topic) of a forum; for forum supergroups and private chats of bots with forum topic mode enabled only
  * `direct_messages_topic_id` - Identifier of the direct messages topic to which the message will be sent; required if the message is sent to a direct messages chat
  * `video` - Video to send. Pass a file_id as String to send a video that exists on the Telegram servers (recommended), pass an HTTP URL as a String for Telegram to get a video from the Internet, or upload a new video using multipart/form-data. More information on Sending Files »
  * `duration` - Duration of sent video in seconds
  * `width` - Video width
  * `height` - Video height
  * `thumbnail` - Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://<file_attach_name>” if the thumbnail was uploaded using multipart/form-data under <file_attach_name>. More information on Sending Files »
  * `cover` - Cover for the video in the message. Pass a file_id to send a file that exists on the Telegram servers (recommended), pass an HTTP URL for Telegram to get a file from the Internet, or pass “attach://<file_attach_name>” to upload a new one using multipart/form-data under <file_attach_name> name. More information on Sending Files »
  * `start_timestamp` - Start timestamp for the video in the message
  * `caption` - Video caption (may also be used when resending videos by file_id), 0-1024 characters after entities parsing
  * `parse_mode` - Mode for parsing entities in the video caption. See formatting options for more details.
  * `caption_entities` - A JSON-serialized list of special entities that appear in the caption, which can be specified instead of parse_mode
  * `show_caption_above_media` - Pass True, if the caption must be shown above the message media
  * `has_spoiler` - Pass True if the video needs to be covered with a spoiler animation
  * `supports_streaming` - Pass True if the uploaded video is suitable for streaming
  * `disable_notification` - Sends the message silently. Users will receive a notification with no sound.
  * `protect_content` - Protects the contents of the sent message from forwarding and saving
  * `allow_paid_broadcast` - Pass True to allow up to 1000 messages per second, ignoring broadcasting limits for a fee of 0.1 Telegram Stars per message. The relevant Stars will be withdrawn from the bot's balance
  * `message_effect_id` - Unique identifier of the message effect to be added to the message; for private chats only
  * `suggested_post_parameters` - A JSON-serialized object containing the parameters of the suggested post to send; for direct messages chats only. If the message is sent as a reply to another suggested post, then that suggested post is automatically declined.
  * `reply_parameters` - Description of the message to reply to
  * `reply_markup` - Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove a reply keyboard or to force a reply from the user
""".
-doc (#{group=><<"Message">>,since=><<"1.15">>}).
-spec sendVideo(Pool :: pool_name(), Req :: #{business_connection_id => binary(), chat_id := integer() | binary(), message_thread_id => integer(), direct_messages_topic_id => integer(), video := 'InputFile'() | binary(), duration => integer(), width => integer(), height => integer(), thumbnail => 'InputFile'() | binary(), cover => 'InputFile'() | binary(), start_timestamp => integer(), caption => binary(), parse_mode => binary(), caption_entities => nonempty_list('MessageEntity'()), show_caption_above_media => boolean(), has_spoiler => boolean(), supports_streaming => boolean(), disable_notification => boolean(), protect_content => boolean(), allow_paid_broadcast => boolean(), message_effect_id => binary(), suggested_post_parameters => 'SuggestedPostParameters'(), reply_parameters => 'ReplyParameters'(), reply_markup => 'InlineKeyboardMarkup'() | 'ReplyKeyboardMarkup'() | 'ReplyKeyboardRemove'() | 'ForceReply'()}, Async :: boolean()) -> Result :: result('Message'()).
sendVideo(Pool, #{chat_id:=_,video:=_} = Req, Async) ->wpool:call(Pool, {multipart, <<"sendVideo">>, Req, Async}).
-doc (#{equiv=>sendVideo(Pool, Req, false),since=><<"1.15">>,group=><<"Sync Request">>}).
sendVideo(Pool, Req) -> sendVideo(Pool, Req, false).


-doc """
Use this method to send animation files (GIF or H.264/MPEG-4 AVC video without sound).  
On success, the sent Message is returned.  
Bots can currently send animation files of up to 50 MB in size, this limit may be changed in the future.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the message will be sent
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_thread_id` - Unique identifier for the target message thread (topic) of a forum; for forum supergroups and private chats of bots with forum topic mode enabled only
  * `direct_messages_topic_id` - Identifier of the direct messages topic to which the message will be sent; required if the message is sent to a direct messages chat
  * `animation` - Animation to send. Pass a file_id as String to send an animation that exists on the Telegram servers (recommended), pass an HTTP URL as a String for Telegram to get an animation from the Internet, or upload a new animation using multipart/form-data. More information on Sending Files »
  * `duration` - Duration of sent animation in seconds
  * `width` - Animation width
  * `height` - Animation height
  * `thumbnail` - Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://<file_attach_name>” if the thumbnail was uploaded using multipart/form-data under <file_attach_name>. More information on Sending Files »
  * `caption` - Animation caption (may also be used when resending animation by file_id), 0-1024 characters after entities parsing
  * `parse_mode` - Mode for parsing entities in the animation caption. See formatting options for more details.
  * `caption_entities` - A JSON-serialized list of special entities that appear in the caption, which can be specified instead of parse_mode
  * `show_caption_above_media` - Pass True, if the caption must be shown above the message media
  * `has_spoiler` - Pass True if the animation needs to be covered with a spoiler animation
  * `disable_notification` - Sends the message silently. Users will receive a notification with no sound.
  * `protect_content` - Protects the contents of the sent message from forwarding and saving
  * `allow_paid_broadcast` - Pass True to allow up to 1000 messages per second, ignoring broadcasting limits for a fee of 0.1 Telegram Stars per message. The relevant Stars will be withdrawn from the bot's balance
  * `message_effect_id` - Unique identifier of the message effect to be added to the message; for private chats only
  * `suggested_post_parameters` - A JSON-serialized object containing the parameters of the suggested post to send; for direct messages chats only. If the message is sent as a reply to another suggested post, then that suggested post is automatically declined.
  * `reply_parameters` - Description of the message to reply to
  * `reply_markup` - Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove a reply keyboard or to force a reply from the user
""".
-doc (#{group=><<"Message">>,since=><<"4.0">>}).
-spec sendAnimation(Pool :: pool_name(), Req :: #{business_connection_id => binary(), chat_id := integer() | binary(), message_thread_id => integer(), direct_messages_topic_id => integer(), animation := 'InputFile'() | binary(), duration => integer(), width => integer(), height => integer(), thumbnail => 'InputFile'() | binary(), caption => binary(), parse_mode => binary(), caption_entities => nonempty_list('MessageEntity'()), show_caption_above_media => boolean(), has_spoiler => boolean(), disable_notification => boolean(), protect_content => boolean(), allow_paid_broadcast => boolean(), message_effect_id => binary(), suggested_post_parameters => 'SuggestedPostParameters'(), reply_parameters => 'ReplyParameters'(), reply_markup => 'InlineKeyboardMarkup'() | 'ReplyKeyboardMarkup'() | 'ReplyKeyboardRemove'() | 'ForceReply'()}, Async :: boolean()) -> Result :: result('Message'()).
sendAnimation(Pool, #{chat_id:=_,animation:=_} = Req, Async) ->wpool:call(Pool, {multipart, <<"sendAnimation">>, Req, Async}).
-doc (#{equiv=>sendAnimation(Pool, Req, false),since=><<"4.0">>,group=><<"Sync Request">>}).
sendAnimation(Pool, Req) -> sendAnimation(Pool, Req, false).


-doc """
Use this method to send audio files, if you want Telegram clients to display the file as a playable voice message.  
For this to work, your audio must be in an .OGG file encoded with OPUS, or in .MP3 format, or in .M4A format (other formats may be sent as Audio or Document).  
On success, the sent Message is returned.  
Bots can currently send voice messages of up to 50 MB in size, this limit may be changed in the future.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the message will be sent
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_thread_id` - Unique identifier for the target message thread (topic) of a forum; for forum supergroups and private chats of bots with forum topic mode enabled only
  * `direct_messages_topic_id` - Identifier of the direct messages topic to which the message will be sent; required if the message is sent to a direct messages chat
  * `voice` - Audio file to send. Pass a file_id as String to send a file that exists on the Telegram servers (recommended), pass an HTTP URL as a String for Telegram to get a file from the Internet, or upload a new one using multipart/form-data. More information on Sending Files »
  * `caption` - Voice message caption, 0-1024 characters after entities parsing
  * `parse_mode` - Mode for parsing entities in the voice message caption. See formatting options for more details.
  * `caption_entities` - A JSON-serialized list of special entities that appear in the caption, which can be specified instead of parse_mode
  * `duration` - Duration of the voice message in seconds
  * `disable_notification` - Sends the message silently. Users will receive a notification with no sound.
  * `protect_content` - Protects the contents of the sent message from forwarding and saving
  * `allow_paid_broadcast` - Pass True to allow up to 1000 messages per second, ignoring broadcasting limits for a fee of 0.1 Telegram Stars per message. The relevant Stars will be withdrawn from the bot's balance
  * `message_effect_id` - Unique identifier of the message effect to be added to the message; for private chats only
  * `suggested_post_parameters` - A JSON-serialized object containing the parameters of the suggested post to send; for direct messages chats only. If the message is sent as a reply to another suggested post, then that suggested post is automatically declined.
  * `reply_parameters` - Description of the message to reply to
  * `reply_markup` - Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove a reply keyboard or to force a reply from the user
""".
-doc (#{group=><<"Message">>,since=><<"1.15">>}).
-spec sendVoice(Pool :: pool_name(), Req :: #{business_connection_id => binary(), chat_id := integer() | binary(), message_thread_id => integer(), direct_messages_topic_id => integer(), voice := 'InputFile'() | binary(), caption => binary(), parse_mode => binary(), caption_entities => nonempty_list('MessageEntity'()), duration => integer(), disable_notification => boolean(), protect_content => boolean(), allow_paid_broadcast => boolean(), message_effect_id => binary(), suggested_post_parameters => 'SuggestedPostParameters'(), reply_parameters => 'ReplyParameters'(), reply_markup => 'InlineKeyboardMarkup'() | 'ReplyKeyboardMarkup'() | 'ReplyKeyboardRemove'() | 'ForceReply'()}, Async :: boolean()) -> Result :: result('Message'()).
sendVoice(Pool, #{chat_id:=_,voice:=_} = Req, Async) ->wpool:call(Pool, {multipart, <<"sendVoice">>, Req, Async}).
-doc (#{equiv=>sendVoice(Pool, Req, false),since=><<"1.15">>,group=><<"Sync Request">>}).
sendVoice(Pool, Req) -> sendVoice(Pool, Req, false).


-doc """
As of v.4.0, Telegram clients support rounded square MPEG4 videos of up to 1 minute long.  
Use this method to send video messages.  
On success, the sent Message is returned.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the message will be sent
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_thread_id` - Unique identifier for the target message thread (topic) of a forum; for forum supergroups and private chats of bots with forum topic mode enabled only
  * `direct_messages_topic_id` - Identifier of the direct messages topic to which the message will be sent; required if the message is sent to a direct messages chat
  * `video_note` - Video note to send. Pass a file_id as String to send a video note that exists on the Telegram servers (recommended) or upload a new video using multipart/form-data. More information on Sending Files ». Sending video notes by a URL is currently unsupported
  * `duration` - Duration of sent video in seconds
  * `length` - Video width and height, i.e. diameter of the video message
  * `thumbnail` - Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://<file_attach_name>” if the thumbnail was uploaded using multipart/form-data under <file_attach_name>. More information on Sending Files »
  * `disable_notification` - Sends the message silently. Users will receive a notification with no sound.
  * `protect_content` - Protects the contents of the sent message from forwarding and saving
  * `allow_paid_broadcast` - Pass True to allow up to 1000 messages per second, ignoring broadcasting limits for a fee of 0.1 Telegram Stars per message. The relevant Stars will be withdrawn from the bot's balance
  * `message_effect_id` - Unique identifier of the message effect to be added to the message; for private chats only
  * `suggested_post_parameters` - A JSON-serialized object containing the parameters of the suggested post to send; for direct messages chats only. If the message is sent as a reply to another suggested post, then that suggested post is automatically declined.
  * `reply_parameters` - Description of the message to reply to
  * `reply_markup` - Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove a reply keyboard or to force a reply from the user
""".
-doc (#{group=><<"Message">>,since=><<"3.0">>}).
-spec sendVideoNote(Pool :: pool_name(), Req :: #{business_connection_id => binary(), chat_id := integer() | binary(), message_thread_id => integer(), direct_messages_topic_id => integer(), video_note := 'InputFile'() | binary(), duration => integer(), length => integer(), thumbnail => 'InputFile'() | binary(), disable_notification => boolean(), protect_content => boolean(), allow_paid_broadcast => boolean(), message_effect_id => binary(), suggested_post_parameters => 'SuggestedPostParameters'(), reply_parameters => 'ReplyParameters'(), reply_markup => 'InlineKeyboardMarkup'() | 'ReplyKeyboardMarkup'() | 'ReplyKeyboardRemove'() | 'ForceReply'()}, Async :: boolean()) -> Result :: result('Message'()).
sendVideoNote(Pool, #{chat_id:=_,video_note:=_} = Req, Async) ->wpool:call(Pool, {multipart, <<"sendVideoNote">>, Req, Async}).
-doc (#{equiv=>sendVideoNote(Pool, Req, false),since=><<"3.0">>,group=><<"Sync Request">>}).
sendVideoNote(Pool, Req) -> sendVideoNote(Pool, Req, false).


-doc """
Use this method to send paid media.  
On success, the sent Message is returned.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the message will be sent
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername). If the chat is a channel, all Telegram Star proceeds from this media will be credited to the chat's balance. Otherwise, they will be credited to the bot's balance.
  * `message_thread_id` - Unique identifier for the target message thread (topic) of a forum; for forum supergroups and private chats of bots with forum topic mode enabled only
  * `direct_messages_topic_id` - Identifier of the direct messages topic to which the message will be sent; required if the message is sent to a direct messages chat
  * `star_count` - The number of Telegram Stars that must be paid to buy access to the media; 1-25000
  * `media` - A JSON-serialized array describing the media to be sent; up to 10 items
  * `payload` - Bot-defined paid media payload, 0-128 bytes. This will not be displayed to the user, use it for your internal processes.
  * `caption` - Media caption, 0-1024 characters after entities parsing
  * `parse_mode` - Mode for parsing entities in the media caption. See formatting options for more details.
  * `caption_entities` - A JSON-serialized list of special entities that appear in the caption, which can be specified instead of parse_mode
  * `show_caption_above_media` - Pass True, if the caption must be shown above the message media
  * `disable_notification` - Sends the message silently. Users will receive a notification with no sound.
  * `protect_content` - Protects the contents of the sent message from forwarding and saving
  * `allow_paid_broadcast` - Pass True to allow up to 1000 messages per second, ignoring broadcasting limits for a fee of 0.1 Telegram Stars per message. The relevant Stars will be withdrawn from the bot's balance
  * `suggested_post_parameters` - A JSON-serialized object containing the parameters of the suggested post to send; for direct messages chats only. If the message is sent as a reply to another suggested post, then that suggested post is automatically declined.
  * `reply_parameters` - Description of the message to reply to
  * `reply_markup` - Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove a reply keyboard or to force a reply from the user
""".
-doc (#{group=><<"Message">>,since=><<"7.6">>}).
-spec sendPaidMedia(Pool :: pool_name(), Req :: #{business_connection_id => binary(), chat_id := integer() | binary(), message_thread_id => integer(), direct_messages_topic_id => integer(), star_count := integer(), media := nonempty_list('InputPaidMedia'()), payload => binary(), caption => binary(), parse_mode => binary(), caption_entities => nonempty_list('MessageEntity'()), show_caption_above_media => boolean(), disable_notification => boolean(), protect_content => boolean(), allow_paid_broadcast => boolean(), suggested_post_parameters => 'SuggestedPostParameters'(), reply_parameters => 'ReplyParameters'(), reply_markup => 'InlineKeyboardMarkup'() | 'ReplyKeyboardMarkup'() | 'ReplyKeyboardRemove'() | 'ForceReply'()}, Async :: boolean()) -> Result :: result('Message'()).
sendPaidMedia(Pool, #{chat_id:=_,star_count:=_,media:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"sendPaidMedia">>, Req, Async}).
-doc (#{equiv=>sendPaidMedia(Pool, Req, false),since=><<"7.6">>,group=><<"Sync Request">>}).
sendPaidMedia(Pool, Req) -> sendPaidMedia(Pool, Req, false).


-doc """
Use this method to send a group of photos, videos, documents or audios as an album.  
Documents and audio files can be only grouped in an album with messages of the same type.  
On success, an array of Message objects that were sent is returned.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the message will be sent
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_thread_id` - Unique identifier for the target message thread (topic) of a forum; for forum supergroups and private chats of bots with forum topic mode enabled only
  * `direct_messages_topic_id` - Identifier of the direct messages topic to which the messages will be sent; required if the messages are sent to a direct messages chat
  * `media` - A JSON-serialized array describing messages to be sent, must include 2-10 items
  * `disable_notification` - Sends messages silently. Users will receive a notification with no sound.
  * `protect_content` - Protects the contents of the sent messages from forwarding and saving
  * `allow_paid_broadcast` - Pass True to allow up to 1000 messages per second, ignoring broadcasting limits for a fee of 0.1 Telegram Stars per message. The relevant Stars will be withdrawn from the bot's balance
  * `message_effect_id` - Unique identifier of the message effect to be added to the message; for private chats only
  * `reply_parameters` - Description of the message to reply to
""".
-doc (#{group=><<"Message">>,since=><<"3.5">>}).
-spec sendMediaGroup(Pool :: pool_name(), Req :: #{business_connection_id => binary(), chat_id := integer() | binary(), message_thread_id => integer(), direct_messages_topic_id => integer(), media := nonempty_list('InputMediaAudio'()) | 'InputMediaDocument'() | 'InputMediaPhoto'() | 'InputMediaVideo'(), disable_notification => boolean(), protect_content => boolean(), allow_paid_broadcast => boolean(), message_effect_id => binary(), reply_parameters => 'ReplyParameters'()}, Async :: boolean()) -> Result :: result(nonempty_list('Message'())).
sendMediaGroup(Pool, #{chat_id:=_,media:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"sendMediaGroup">>, Req, Async}).
-doc (#{equiv=>sendMediaGroup(Pool, Req, false),since=><<"3.5">>,group=><<"Sync Request">>}).
sendMediaGroup(Pool, Req) -> sendMediaGroup(Pool, Req, false).


-doc """
Use this method to send point on the map.  
On success, the sent Message is returned.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the message will be sent
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_thread_id` - Unique identifier for the target message thread (topic) of a forum; for forum supergroups and private chats of bots with forum topic mode enabled only
  * `direct_messages_topic_id` - Identifier of the direct messages topic to which the message will be sent; required if the message is sent to a direct messages chat
  * `latitude` - Latitude of the location
  * `longitude` - Longitude of the location
  * `horizontal_accuracy` - The radius of uncertainty for the location, measured in meters; 0-1500
  * `live_period` - Period in seconds during which the location will be updated (see Live Locations, should be between 60 and 86400, or 0x7FFFFFFF for live locations that can be edited indefinitely.
  * `heading` - For live locations, a direction in which the user is moving, in degrees. Must be between 1 and 360 if specified.
  * `proximity_alert_radius` - For live locations, a maximum distance for proximity alerts about approaching another chat member, in meters. Must be between 1 and 100000 if specified.
  * `disable_notification` - Sends the message silently. Users will receive a notification with no sound.
  * `protect_content` - Protects the contents of the sent message from forwarding and saving
  * `allow_paid_broadcast` - Pass True to allow up to 1000 messages per second, ignoring broadcasting limits for a fee of 0.1 Telegram Stars per message. The relevant Stars will be withdrawn from the bot's balance
  * `message_effect_id` - Unique identifier of the message effect to be added to the message; for private chats only
  * `suggested_post_parameters` - A JSON-serialized object containing the parameters of the suggested post to send; for direct messages chats only. If the message is sent as a reply to another suggested post, then that suggested post is automatically declined.
  * `reply_parameters` - Description of the message to reply to
  * `reply_markup` - Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove a reply keyboard or to force a reply from the user
""".
-doc (#{group=><<"Message">>,since=><<"3.4">>}).
-spec sendLocation(Pool :: pool_name(), Req :: #{business_connection_id => binary(), chat_id := integer() | binary(), message_thread_id => integer(), direct_messages_topic_id => integer(), latitude := float(), longitude := float(), horizontal_accuracy => float(), live_period => integer(), heading => integer(), proximity_alert_radius => integer(), disable_notification => boolean(), protect_content => boolean(), allow_paid_broadcast => boolean(), message_effect_id => binary(), suggested_post_parameters => 'SuggestedPostParameters'(), reply_parameters => 'ReplyParameters'(), reply_markup => 'InlineKeyboardMarkup'() | 'ReplyKeyboardMarkup'() | 'ReplyKeyboardRemove'() | 'ForceReply'()}, Async :: boolean()) -> Result :: result('Message'()).
sendLocation(Pool, #{chat_id:=_,latitude:=_,longitude:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"sendLocation">>, Req, Async}).
-doc (#{equiv=>sendLocation(Pool, Req, false),since=><<"3.4">>,group=><<"Sync Request">>}).
sendLocation(Pool, Req) -> sendLocation(Pool, Req, false).


-doc """
Use this method to send information about a venue.  
On success, the sent Message is returned.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the message will be sent
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_thread_id` - Unique identifier for the target message thread (topic) of a forum; for forum supergroups and private chats of bots with forum topic mode enabled only
  * `direct_messages_topic_id` - Identifier of the direct messages topic to which the message will be sent; required if the message is sent to a direct messages chat
  * `latitude` - Latitude of the venue
  * `longitude` - Longitude of the venue
  * `title` - Name of the venue
  * `address` - Address of the venue
  * `foursquare_id` - Foursquare identifier of the venue
  * `foursquare_type` - Foursquare type of the venue, if known. (For example, “arts_entertainment/default”, “arts_entertainment/aquarium” or “food/icecream”.)
  * `google_place_id` - Google Places identifier of the venue
  * `google_place_type` - Google Places type of the venue. (See supported types.)
  * `disable_notification` - Sends the message silently. Users will receive a notification with no sound.
  * `protect_content` - Protects the contents of the sent message from forwarding and saving
  * `allow_paid_broadcast` - Pass True to allow up to 1000 messages per second, ignoring broadcasting limits for a fee of 0.1 Telegram Stars per message. The relevant Stars will be withdrawn from the bot's balance
  * `message_effect_id` - Unique identifier of the message effect to be added to the message; for private chats only
  * `suggested_post_parameters` - A JSON-serialized object containing the parameters of the suggested post to send; for direct messages chats only. If the message is sent as a reply to another suggested post, then that suggested post is automatically declined.
  * `reply_parameters` - Description of the message to reply to
  * `reply_markup` - Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove a reply keyboard or to force a reply from the user
""".
-doc (#{group=><<"Message">>,since=><<"2.0">>}).
-spec sendVenue(Pool :: pool_name(), Req :: #{business_connection_id => binary(), chat_id := integer() | binary(), message_thread_id => integer(), direct_messages_topic_id => integer(), latitude := float(), longitude := float(), title := binary(), address := binary(), foursquare_id => binary(), foursquare_type => binary(), google_place_id => binary(), google_place_type => binary(), disable_notification => boolean(), protect_content => boolean(), allow_paid_broadcast => boolean(), message_effect_id => binary(), suggested_post_parameters => 'SuggestedPostParameters'(), reply_parameters => 'ReplyParameters'(), reply_markup => 'InlineKeyboardMarkup'() | 'ReplyKeyboardMarkup'() | 'ReplyKeyboardRemove'() | 'ForceReply'()}, Async :: boolean()) -> Result :: result('Message'()).
sendVenue(Pool, #{chat_id:=_,latitude:=_,longitude:=_,title:=_,address:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"sendVenue">>, Req, Async}).
-doc (#{equiv=>sendVenue(Pool, Req, false),since=><<"2.0">>,group=><<"Sync Request">>}).
sendVenue(Pool, Req) -> sendVenue(Pool, Req, false).


-doc """
Use this method to send phone contacts.  
On success, the sent Message is returned.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the message will be sent
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_thread_id` - Unique identifier for the target message thread (topic) of a forum; for forum supergroups and private chats of bots with forum topic mode enabled only
  * `direct_messages_topic_id` - Identifier of the direct messages topic to which the message will be sent; required if the message is sent to a direct messages chat
  * `phone_number` - Contact's phone number
  * `first_name` - Contact's first name
  * `last_name` - Contact's last name
  * `vcard` - Additional data about the contact in the form of a vCard, 0-2048 bytes
  * `disable_notification` - Sends the message silently. Users will receive a notification with no sound.
  * `protect_content` - Protects the contents of the sent message from forwarding and saving
  * `allow_paid_broadcast` - Pass True to allow up to 1000 messages per second, ignoring broadcasting limits for a fee of 0.1 Telegram Stars per message. The relevant Stars will be withdrawn from the bot's balance
  * `message_effect_id` - Unique identifier of the message effect to be added to the message; for private chats only
  * `suggested_post_parameters` - A JSON-serialized object containing the parameters of the suggested post to send; for direct messages chats only. If the message is sent as a reply to another suggested post, then that suggested post is automatically declined.
  * `reply_parameters` - Description of the message to reply to
  * `reply_markup` - Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove a reply keyboard or to force a reply from the user
""".
-doc (#{group=><<"Message">>,since=><<"2.0">>}).
-spec sendContact(Pool :: pool_name(), Req :: #{business_connection_id => binary(), chat_id := integer() | binary(), message_thread_id => integer(), direct_messages_topic_id => integer(), phone_number := binary(), first_name := binary(), last_name => binary(), vcard => binary(), disable_notification => boolean(), protect_content => boolean(), allow_paid_broadcast => boolean(), message_effect_id => binary(), suggested_post_parameters => 'SuggestedPostParameters'(), reply_parameters => 'ReplyParameters'(), reply_markup => 'InlineKeyboardMarkup'() | 'ReplyKeyboardMarkup'() | 'ReplyKeyboardRemove'() | 'ForceReply'()}, Async :: boolean()) -> Result :: result('Message'()).
sendContact(Pool, #{chat_id:=_,phone_number:=_,first_name:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"sendContact">>, Req, Async}).
-doc (#{equiv=>sendContact(Pool, Req, false),since=><<"2.0">>,group=><<"Sync Request">>}).
sendContact(Pool, Req) -> sendContact(Pool, Req, false).


-doc """
Use this method to send a native poll.  
On success, the sent Message is returned.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the message will be sent
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername). Polls can't be sent to channel direct messages chats.
  * `message_thread_id` - Unique identifier for the target message thread (topic) of a forum; for forum supergroups and private chats of bots with forum topic mode enabled only
  * `question` - Poll question, 1-300 characters
  * `question_parse_mode` - Mode for parsing entities in the question. See formatting options for more details. Currently, only custom emoji entities are allowed
  * `question_entities` - A JSON-serialized list of special entities that appear in the poll question. It can be specified instead of question_parse_mode
  * `options` - A JSON-serialized list of 2-12 answer options
  * `is_anonymous` - True, if the poll needs to be anonymous, defaults to True
  * `type` - Poll type, “quiz” or “regular”, defaults to “regular”
  * `allows_multiple_answers` - True, if the poll allows multiple answers, ignored for polls in quiz mode, defaults to False
  * `correct_option_id` - 0-based identifier of the correct answer option, required for polls in quiz mode
  * `explanation` - Text that is shown when a user chooses an incorrect answer or taps on the lamp icon in a quiz-style poll, 0-200 characters with at most 2 line feeds after entities parsing
  * `explanation_parse_mode` - Mode for parsing entities in the explanation. See formatting options for more details.
  * `explanation_entities` - A JSON-serialized list of special entities that appear in the poll explanation. It can be specified instead of explanation_parse_mode
  * `open_period` - Amount of time in seconds the poll will be active after creation, 5-600. Can't be used together with close_date.
  * `close_date` - Point in time (Unix timestamp) when the poll will be automatically closed. Must be at least 5 and no more than 600 seconds in the future. Can't be used together with open_period.
  * `is_closed` - Pass True if the poll needs to be immediately closed. This can be useful for poll preview.
  * `disable_notification` - Sends the message silently. Users will receive a notification with no sound.
  * `protect_content` - Protects the contents of the sent message from forwarding and saving
  * `allow_paid_broadcast` - Pass True to allow up to 1000 messages per second, ignoring broadcasting limits for a fee of 0.1 Telegram Stars per message. The relevant Stars will be withdrawn from the bot's balance
  * `message_effect_id` - Unique identifier of the message effect to be added to the message; for private chats only
  * `reply_parameters` - Description of the message to reply to
  * `reply_markup` - Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove a reply keyboard or to force a reply from the user
""".
-doc (#{group=><<"Message">>,since=><<"4.2">>}).
-spec sendPoll(Pool :: pool_name(), Req :: #{business_connection_id => binary(), chat_id := integer() | binary(), message_thread_id => integer(), question := binary(), question_parse_mode => binary(), question_entities => nonempty_list('MessageEntity'()), options := nonempty_list('InputPollOption'()), is_anonymous => boolean(), type => binary(), allows_multiple_answers => boolean(), correct_option_id => integer(), explanation => binary(), explanation_parse_mode => binary(), explanation_entities => nonempty_list('MessageEntity'()), open_period => integer(), close_date => integer(), is_closed => boolean(), disable_notification => boolean(), protect_content => boolean(), allow_paid_broadcast => boolean(), message_effect_id => binary(), reply_parameters => 'ReplyParameters'(), reply_markup => 'InlineKeyboardMarkup'() | 'ReplyKeyboardMarkup'() | 'ReplyKeyboardRemove'() | 'ForceReply'()}, Async :: boolean()) -> Result :: result('Message'()).
sendPoll(Pool, #{chat_id:=_,question:=_,options:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"sendPoll">>, Req, Async}).
-doc (#{equiv=>sendPoll(Pool, Req, false),since=><<"4.2">>,group=><<"Sync Request">>}).
sendPoll(Pool, Req) -> sendPoll(Pool, Req, false).


-doc """
Use this method to send a checklist on behalf of a connected business account.  
On success, the sent Message is returned.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the message will be sent
  * `chat_id` - Unique identifier for the target chat
  * `checklist` - A JSON-serialized object for the checklist to send
  * `disable_notification` - Sends the message silently. Users will receive a notification with no sound.
  * `protect_content` - Protects the contents of the sent message from forwarding and saving
  * `message_effect_id` - Unique identifier of the message effect to be added to the message
  * `reply_parameters` - A JSON-serialized object for description of the message to reply to
  * `reply_markup` - A JSON-serialized object for an inline keyboard
""".
-doc (#{group=><<"Message">>,since=><<"9.1">>}).
-spec sendChecklist(Pool :: pool_name(), Req :: #{business_connection_id := binary(), chat_id := integer(), checklist := 'InputChecklist'(), disable_notification => boolean(), protect_content => boolean(), message_effect_id => binary(), reply_parameters => 'ReplyParameters'(), reply_markup => 'InlineKeyboardMarkup'()}, Async :: boolean()) -> Result :: result('Message'()).
sendChecklist(Pool, #{business_connection_id:=_,chat_id:=_,checklist:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"sendChecklist">>, Req, Async}).
-doc (#{equiv=>sendChecklist(Pool, Req, false),since=><<"9.1">>,group=><<"Sync Request">>}).
sendChecklist(Pool, Req) -> sendChecklist(Pool, Req, false).


-doc """
Use this method to send an animated emoji that will display a random value.  
On success, the sent Message is returned.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the message will be sent
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_thread_id` - Unique identifier for the target message thread (topic) of a forum; for forum supergroups and private chats of bots with forum topic mode enabled only
  * `direct_messages_topic_id` - Identifier of the direct messages topic to which the message will be sent; required if the message is sent to a direct messages chat
  * `emoji` - Emoji on which the dice throw animation is based. Currently, must be one of “”, “”, “”, “”, “”, or “”. Dice can have values 1-6 for “”, “” and “”, values 1-5 for “” and “”, and values 1-64 for “”. Defaults to “”
  * `disable_notification` - Sends the message silently. Users will receive a notification with no sound.
  * `protect_content` - Protects the contents of the sent message from forwarding
  * `allow_paid_broadcast` - Pass True to allow up to 1000 messages per second, ignoring broadcasting limits for a fee of 0.1 Telegram Stars per message. The relevant Stars will be withdrawn from the bot's balance
  * `message_effect_id` - Unique identifier of the message effect to be added to the message; for private chats only
  * `suggested_post_parameters` - A JSON-serialized object containing the parameters of the suggested post to send; for direct messages chats only. If the message is sent as a reply to another suggested post, then that suggested post is automatically declined.
  * `reply_parameters` - Description of the message to reply to
  * `reply_markup` - Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove a reply keyboard or to force a reply from the user
""".
-doc (#{group=><<"Message">>,since=><<"4.7">>}).
-spec sendDice(Pool :: pool_name(), Req :: #{business_connection_id => binary(), chat_id := integer() | binary(), message_thread_id => integer(), direct_messages_topic_id => integer(), emoji => binary(), disable_notification => boolean(), protect_content => boolean(), allow_paid_broadcast => boolean(), message_effect_id => binary(), suggested_post_parameters => 'SuggestedPostParameters'(), reply_parameters => 'ReplyParameters'(), reply_markup => 'InlineKeyboardMarkup'() | 'ReplyKeyboardMarkup'() | 'ReplyKeyboardRemove'() | 'ForceReply'()}, Async :: boolean()) -> Result :: result('Message'()).
sendDice(Pool, #{chat_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"sendDice">>, Req, Async}).
-doc (#{equiv=>sendDice(Pool, Req, false),since=><<"4.7">>,group=><<"Sync Request">>}).
sendDice(Pool, Req) -> sendDice(Pool, Req, false).


-doc """
Use this method to stream a partial message to a user while the message is being generated; supported only for bots with forum topic mode enabled.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target private chat
  * `message_thread_id` - Unique identifier for the target message thread
  * `draft_id` - Unique identifier of the message draft; must be non-zero. Changes of drafts with the same identifier are animated
  * `text` - Text of the message to be sent, 1-4096 characters after entities parsing
  * `parse_mode` - Mode for parsing entities in the message text. See formatting options for more details.
  * `entities` - A JSON-serialized list of special entities that appear in message text, which can be specified instead of parse_mode
""".
-doc (#{group=><<"Message">>,since=><<"9.3">>}).
-spec sendMessageDraft(Pool :: pool_name(), Req :: #{chat_id := integer(), message_thread_id => integer(), draft_id := integer(), text := binary(), parse_mode => binary(), entities => nonempty_list('MessageEntity'())}, Async :: boolean()) -> Result :: result(true).
sendMessageDraft(Pool, #{chat_id:=_,draft_id:=_,text:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"sendMessageDraft">>, Req, Async}).
-doc (#{equiv=>sendMessageDraft(Pool, Req, false),since=><<"9.3">>,group=><<"Sync Request">>}).
sendMessageDraft(Pool, Req) -> sendMessageDraft(Pool, Req, false).


-doc """
Use this method when you need to tell the user that something is happening on the bot's side.  
The status is set for 5 seconds or less (when a message arrives from your bot, Telegram clients clear its typing status).  
Returns True on success.  
We only recommend using this method when a response from the bot will take a noticeable amount of time to arrive.  
Example: The ImageBot needs some time to process a request and upload the image.  
Instead of sending a text message along the lines of “Retrieving image, please wait…”, the bot may use sendChatAction with action = upload_photo.  
The user will see a “sending photo” status for the bot.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the action will be sent
  * `chat_id` - Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername). Channel chats and channel direct messages chats aren't supported.
  * `message_thread_id` - Unique identifier for the target message thread or topic of a forum; for supergroups and private chats of bots with forum topic mode enabled only
  * `action` - Type of action to broadcast. Choose one, depending on what the user is about to receive: typing for text messages, upload_photo for photos, record_video or upload_video for videos, record_voice or upload_voice for voice notes, upload_document for general files, choose_sticker for stickers, find_location for location data, record_video_note or upload_video_note for video notes.
""".
-doc (#{group=><<"Message">>,since=><<"3.0">>}).
-spec sendChatAction(Pool :: pool_name(), Req :: #{business_connection_id => binary(), chat_id := integer() | binary(), message_thread_id => integer(), action := binary()}, Async :: boolean()) -> Result :: result(true).
sendChatAction(Pool, #{chat_id:=_,action:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"sendChatAction">>, Req, Async}).
-doc (#{equiv=>sendChatAction(Pool, Req, false),since=><<"3.0">>,group=><<"Sync Request">>}).
sendChatAction(Pool, Req) -> sendChatAction(Pool, Req, false).


-doc """
Use this method to change the chosen reactions on a message.  
Service messages of some types can't be reacted to.  
Automatically forwarded messages from a channel to its discussion group have the same available reactions as messages in the channel.  
Bots can't use paid reactions.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_id` - Identifier of the target message. If the message belongs to a media group, the reaction is set to the first non-deleted message in the group instead.
  * `reaction` - A JSON-serialized list of reaction types to set on the message. Currently, as non-premium users, bots can set up to one reaction per message. A custom emoji reaction can be used if it is either already present on the message or explicitly allowed by chat administrators. Paid reactions can't be used by bots.
  * `is_big` - Pass True to set the reaction with a big animation
""".
-doc (#{group=><<"Message">>,since=><<"7.0">>}).
-spec setMessageReaction(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), message_id := integer(), reaction => nonempty_list('ReactionType'()), is_big => boolean()}, Async :: boolean()) -> Result :: result(true).
setMessageReaction(Pool, #{chat_id:=_,message_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"setMessageReaction">>, Req, Async}).
-doc (#{equiv=>setMessageReaction(Pool, Req, false),since=><<"7.0">>,group=><<"Sync Request">>}).
setMessageReaction(Pool, Req) -> setMessageReaction(Pool, Req, false).


-doc """
Use this method to get a list of profile pictures for a user.  
Returns a UserProfilePhotos object.
## Parameters
  * `user_id` - Unique identifier of the target user
  * `offset` - Sequential number of the first photo to be returned. By default, all photos are returned.
  * `limit` - Limits the number of photos to be retrieved. Values between 1-100 are accepted. Defaults to 100.
""".
-doc (#{group=><<"User">>,since=><<"">>}).
-spec getUserProfilePhotos(Pool :: pool_name(), Req :: #{user_id := integer(), offset => integer(), limit => integer()}, Async :: boolean()) -> Result :: result('UserProfilePhotos'()).
getUserProfilePhotos(Pool, #{user_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"getUserProfilePhotos">>, Req, Async}).
-doc (#{equiv=>getUserProfilePhotos(Pool, Req, false),since=><<"">>,group=><<"Sync Request">>}).
getUserProfilePhotos(Pool, Req) -> getUserProfilePhotos(Pool, Req, false).


-doc """
Changes the emoji status for a given user that previously allowed the bot to manage their emoji status via the Mini App method requestEmojiStatusAccess.  
Returns True on success.
## Parameters
  * `user_id` - Unique identifier of the target user
  * `emoji_status_custom_emoji_id` - Custom emoji identifier of the emoji status to set. Pass an empty string to remove the status.
  * `emoji_status_expiration_date` - Expiration date of the emoji status, if any
""".
-doc (#{group=><<"User">>,since=><<"8.0">>}).
-spec setUserEmojiStatus(Pool :: pool_name(), Req :: #{user_id := integer(), emoji_status_custom_emoji_id => binary(), emoji_status_expiration_date => integer()}, Async :: boolean()) -> Result :: result(true).
setUserEmojiStatus(Pool, #{user_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"setUserEmojiStatus">>, Req, Async}).
-doc (#{equiv=>setUserEmojiStatus(Pool, Req, false),since=><<"8.0">>,group=><<"Sync Request">>}).
setUserEmojiStatus(Pool, Req) -> setUserEmojiStatus(Pool, Req, false).


-doc """
Use this method to get basic information about a file and prepare it for downloading.  
For the moment, bots can download files of up to 20MB in size.  
On success, a File object is returned.  
The file can then be downloaded via the link https://api.telegram.org/file/bot<token>/<file_path>, where <file_path> is taken from the response.  
It is guaranteed that the link will be valid for at least 1 hour.  
When the link expires, a new one can be requested by calling getFile again.  
Note: This function may not preserve the original file name and MIME type.  
You should save the file's MIME type and name (if available) when the File object is received.
## Parameters
  * `file_id` - File identifier to get information about
""".
-doc (#{group=><<"File">>,since=><<"1.15">>}).
-spec getFile(Pool :: pool_name(), Req :: #{file_id := binary()}, Async :: boolean()) -> Result :: result('File'()).
getFile(Pool, #{file_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"getFile">>, Req, Async}).
-doc (#{equiv=>getFile(Pool, Req, false),since=><<"1.15">>,group=><<"Sync Request">>}).
getFile(Pool, Req) -> getFile(Pool, Req, false).


-doc """
Use this method to ban a user in a group, a supergroup or a channel.  
In the case of supergroups and channels, the user will not be able to return to the chat on their own using invite links, etc., unless unbanned first.  
The bot must be an administrator in the chat for this to work and must have the appropriate administrator rights.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target group or username of the target supergroup or channel (in the format @channelusername)
  * `user_id` - Unique identifier of the target user
  * `until_date` - Date when the user will be unbanned; Unix time. If user is banned for more than 366 days or less than 30 seconds from the current time they are considered to be banned forever. Applied for supergroups and channels only.
  * `revoke_messages` - Pass True to delete all messages from the chat for the user that is being removed. If False, the user will be able to see messages in the group that were sent before the user was removed. Always True for supergroups and channels.
""".
-doc (#{group=><<"Chat">>,since=><<"2.0">>}).
-spec banChatMember(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), user_id := integer(), until_date => integer(), revoke_messages => boolean()}, Async :: boolean()) -> Result :: result(true).
banChatMember(Pool, #{chat_id:=_,user_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"banChatMember">>, Req, Async}).
-doc (#{equiv=>banChatMember(Pool, Req, false),since=><<"2.0">>,group=><<"Sync Request">>}).
banChatMember(Pool, Req) -> banChatMember(Pool, Req, false).


-doc """
Use this method to unban a previously banned user in a supergroup or channel.  
The user will not return to the group or channel automatically, but will be able to join via link, etc.  
The bot must be an administrator for this to work.  
By default, this method guarantees that after the call the user is not a member of the chat, but will be able to join it.  
So if the user is a member of the chat they will also be removed from the chat.  
If you don't want this, use the parameter only_if_banned.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target group or username of the target supergroup or channel (in the format @channelusername)
  * `user_id` - Unique identifier of the target user
  * `only_if_banned` - Do nothing if the user is not banned
""".
-doc (#{group=><<"Chat">>,since=><<"2.0">>}).
-spec unbanChatMember(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), user_id := integer(), only_if_banned => boolean()}, Async :: boolean()) -> Result :: result(true).
unbanChatMember(Pool, #{chat_id:=_,user_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"unbanChatMember">>, Req, Async}).
-doc (#{equiv=>unbanChatMember(Pool, Req, false),since=><<"2.0">>,group=><<"Sync Request">>}).
unbanChatMember(Pool, Req) -> unbanChatMember(Pool, Req, false).


-doc """
Use this method to restrict a user in a supergroup.  
The bot must be an administrator in the supergroup for this to work and must have the appropriate administrator rights.  
Pass True for all permissions to lift restrictions from a user.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  * `user_id` - Unique identifier of the target user
  * `permissions` - A JSON-serialized object for new user permissions
  * `use_independent_chat_permissions` - Pass True if chat permissions are set independently. Otherwise, the can_send_other_messages and can_add_web_page_previews permissions will imply the can_send_messages, can_send_audios, can_send_documents, can_send_photos, can_send_videos, can_send_video_notes, and can_send_voice_notes permissions; the can_send_polls permission will imply the can_send_messages permission.
  * `until_date` - Date when restrictions will be lifted for the user; Unix time. If user is restricted for more than 366 days or less than 30 seconds from the current time, they are considered to be restricted forever
""".
-doc (#{group=><<"Chat">>,since=><<"3.1">>}).
-spec restrictChatMember(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), user_id := integer(), permissions := 'ChatPermissions'(), use_independent_chat_permissions => boolean(), until_date => integer()}, Async :: boolean()) -> Result :: result(true).
restrictChatMember(Pool, #{chat_id:=_,user_id:=_,permissions:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"restrictChatMember">>, Req, Async}).
-doc (#{equiv=>restrictChatMember(Pool, Req, false),since=><<"3.1">>,group=><<"Sync Request">>}).
restrictChatMember(Pool, Req) -> restrictChatMember(Pool, Req, false).


-doc """
Use this method to promote or demote a user in a supergroup or a channel.  
The bot must be an administrator in the chat for this to work and must have the appropriate administrator rights.  
Pass False for all boolean parameters to demote a user.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `user_id` - Unique identifier of the target user
  * `is_anonymous` - Pass True if the administrator's presence in the chat is hidden
  * `can_manage_chat` - Pass True if the administrator can access the chat event log, get boost list, see hidden supergroup and channel members, report spam messages, ignore slow mode, and send messages to the chat without paying Telegram Stars. Implied by any other administrator privilege.
  * `can_delete_messages` - Pass True if the administrator can delete messages of other users
  * `can_manage_video_chats` - Pass True if the administrator can manage video chats
  * `can_restrict_members` - Pass True if the administrator can restrict, ban or unban chat members, or access supergroup statistics. For backward compatibility, defaults to True for promotions of channel administrators
  * `can_promote_members` - Pass True if the administrator can add new administrators with a subset of their own privileges or demote administrators that they have promoted, directly or indirectly (promoted by administrators that were appointed by him)
  * `can_change_info` - Pass True if the administrator can change chat title, photo and other settings
  * `can_invite_users` - Pass True if the administrator can invite new users to the chat
  * `can_post_stories` - Pass True if the administrator can post stories to the chat
  * `can_edit_stories` - Pass True if the administrator can edit stories posted by other users, post stories to the chat page, pin chat stories, and access the chat's story archive
  * `can_delete_stories` - Pass True if the administrator can delete stories posted by other users
  * `can_post_messages` - Pass True if the administrator can post messages in the channel, approve suggested posts, or access channel statistics; for channels only
  * `can_edit_messages` - Pass True if the administrator can edit messages of other users and can pin messages; for channels only
  * `can_pin_messages` - Pass True if the administrator can pin messages; for supergroups only
  * `can_manage_topics` - Pass True if the user is allowed to create, rename, close, and reopen forum topics; for supergroups only
  * `can_manage_direct_messages` - Pass True if the administrator can manage direct messages within the channel and decline suggested posts; for channels only
""".
-doc (#{group=><<"Chat">>,since=><<"3.1">>}).
-spec promoteChatMember(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), user_id := integer(), is_anonymous => boolean(), can_manage_chat => boolean(), can_delete_messages => boolean(), can_manage_video_chats => boolean(), can_restrict_members => boolean(), can_promote_members => boolean(), can_change_info => boolean(), can_invite_users => boolean(), can_post_stories => boolean(), can_edit_stories => boolean(), can_delete_stories => boolean(), can_post_messages => boolean(), can_edit_messages => boolean(), can_pin_messages => boolean(), can_manage_topics => boolean(), can_manage_direct_messages => boolean()}, Async :: boolean()) -> Result :: result(true).
promoteChatMember(Pool, #{chat_id:=_,user_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"promoteChatMember">>, Req, Async}).
-doc (#{equiv=>promoteChatMember(Pool, Req, false),since=><<"3.1">>,group=><<"Sync Request">>}).
promoteChatMember(Pool, Req) -> promoteChatMember(Pool, Req, false).


-doc """
Use this method to set a custom title for an administrator in a supergroup promoted by the bot.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  * `user_id` - Unique identifier of the target user
  * `custom_title` - New custom title for the administrator; 0-16 characters, emoji are not allowed
""".
-doc (#{group=><<"Chat">>,since=><<"4.5">>}).
-spec setChatAdministratorCustomTitle(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), user_id := integer(), custom_title := binary()}, Async :: boolean()) -> Result :: result(true).
setChatAdministratorCustomTitle(Pool, #{chat_id:=_,user_id:=_,custom_title:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"setChatAdministratorCustomTitle">>, Req, Async}).
-doc (#{equiv=>setChatAdministratorCustomTitle(Pool, Req, false),since=><<"4.5">>,group=><<"Sync Request">>}).
setChatAdministratorCustomTitle(Pool, Req) -> setChatAdministratorCustomTitle(Pool, Req, false).


-doc """
Use this method to ban a channel chat in a supergroup or a channel.  
Until the chat is unbanned, the owner of the banned chat won't be able to send messages on behalf of any of their channels.  
The bot must be an administrator in the supergroup or channel for this to work and must have the appropriate administrator rights.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `sender_chat_id` - Unique identifier of the target sender chat
""".
-doc (#{group=><<"Chat">>,since=><<"5.5">>}).
-spec banChatSenderChat(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), sender_chat_id := integer()}, Async :: boolean()) -> Result :: result(true).
banChatSenderChat(Pool, #{chat_id:=_,sender_chat_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"banChatSenderChat">>, Req, Async}).
-doc (#{equiv=>banChatSenderChat(Pool, Req, false),since=><<"5.5">>,group=><<"Sync Request">>}).
banChatSenderChat(Pool, Req) -> banChatSenderChat(Pool, Req, false).


-doc """
Use this method to unban a previously banned channel chat in a supergroup or channel.  
The bot must be an administrator for this to work and must have the appropriate administrator rights.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `sender_chat_id` - Unique identifier of the target sender chat
""".
-doc (#{group=><<"Chat">>,since=><<"5.5">>}).
-spec unbanChatSenderChat(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), sender_chat_id := integer()}, Async :: boolean()) -> Result :: result(true).
unbanChatSenderChat(Pool, #{chat_id:=_,sender_chat_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"unbanChatSenderChat">>, Req, Async}).
-doc (#{equiv=>unbanChatSenderChat(Pool, Req, false),since=><<"5.5">>,group=><<"Sync Request">>}).
unbanChatSenderChat(Pool, Req) -> unbanChatSenderChat(Pool, Req, false).


-doc """
Use this method to set default chat permissions for all members.  
The bot must be an administrator in the group or a supergroup for this to work and must have the can_restrict_members administrator rights.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  * `permissions` - A JSON-serialized object for new default chat permissions
  * `use_independent_chat_permissions` - Pass True if chat permissions are set independently. Otherwise, the can_send_other_messages and can_add_web_page_previews permissions will imply the can_send_messages, can_send_audios, can_send_documents, can_send_photos, can_send_videos, can_send_video_notes, and can_send_voice_notes permissions; the can_send_polls permission will imply the can_send_messages permission.
""".
-doc (#{group=><<"Chat">>,since=><<"4.4">>}).
-spec setChatPermissions(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), permissions := 'ChatPermissions'(), use_independent_chat_permissions => boolean()}, Async :: boolean()) -> Result :: result(true).
setChatPermissions(Pool, #{chat_id:=_,permissions:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"setChatPermissions">>, Req, Async}).
-doc (#{equiv=>setChatPermissions(Pool, Req, false),since=><<"4.4">>,group=><<"Sync Request">>}).
setChatPermissions(Pool, Req) -> setChatPermissions(Pool, Req, false).


-doc """
Use this method to generate a new primary invite link for a chat; any previously generated primary link is revoked.  
The bot must be an administrator in the chat for this to work and must have the appropriate administrator rights.  
Returns the new invite link as String on success.  
Note: Each administrator in a chat generates their own invite links.  
Bots can't use invite links generated by other administrators.  
If you want your bot to work with invite links, it will need to generate its own link using exportChatInviteLink or by calling the getChat method.  
If your bot needs to generate a new primary invite link replacing its previous one, use exportChatInviteLink again.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
""".
-doc (#{group=><<"Chat invite">>,since=><<"3.1">>}).
-spec exportChatInviteLink(Pool :: pool_name(), Req :: #{chat_id := integer() | binary()}, Async :: boolean()) -> Result :: result(binary()).
exportChatInviteLink(Pool, #{chat_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"exportChatInviteLink">>, Req, Async}).
-doc (#{equiv=>exportChatInviteLink(Pool, Req, false),since=><<"3.1">>,group=><<"Sync Request">>}).
exportChatInviteLink(Pool, Req) -> exportChatInviteLink(Pool, Req, false).


-doc """
Use this method to create an additional invite link for a chat.  
The bot must be an administrator in the chat for this to work and must have the appropriate administrator rights.  
The link can be revoked using the method revokeChatInviteLink.  
Returns the new invite link as ChatInviteLink object.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `name` - Invite link name; 0-32 characters
  * `expire_date` - Point in time (Unix timestamp) when the link will expire
  * `member_limit` - The maximum number of users that can be members of the chat simultaneously after joining the chat via this invite link; 1-99999
  * `creates_join_request` - True, if users joining the chat via the link need to be approved by chat administrators. If True, member_limit can't be specified
""".
-doc (#{group=><<"Chat invite">>,since=><<"5.1">>}).
-spec createChatInviteLink(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), name => binary(), expire_date => integer(), member_limit => integer(), creates_join_request => boolean()}, Async :: boolean()) -> Result :: result('ChatInviteLink'()).
createChatInviteLink(Pool, #{chat_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"createChatInviteLink">>, Req, Async}).
-doc (#{equiv=>createChatInviteLink(Pool, Req, false),since=><<"5.1">>,group=><<"Sync Request">>}).
createChatInviteLink(Pool, Req) -> createChatInviteLink(Pool, Req, false).


-doc """
Use this method to edit a non-primary invite link created by the bot.  
The bot must be an administrator in the chat for this to work and must have the appropriate administrator rights.  
Returns the edited invite link as a ChatInviteLink object.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `invite_link` - The invite link to edit
  * `name` - Invite link name; 0-32 characters
  * `expire_date` - Point in time (Unix timestamp) when the link will expire
  * `member_limit` - The maximum number of users that can be members of the chat simultaneously after joining the chat via this invite link; 1-99999
  * `creates_join_request` - True, if users joining the chat via the link need to be approved by chat administrators. If True, member_limit can't be specified
""".
-doc (#{group=><<"Chat invite">>,since=><<"5.1">>}).
-spec editChatInviteLink(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), invite_link := binary(), name => binary(), expire_date => integer(), member_limit => integer(), creates_join_request => boolean()}, Async :: boolean()) -> Result :: result('ChatInviteLink'()).
editChatInviteLink(Pool, #{chat_id:=_,invite_link:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"editChatInviteLink">>, Req, Async}).
-doc (#{equiv=>editChatInviteLink(Pool, Req, false),since=><<"5.1">>,group=><<"Sync Request">>}).
editChatInviteLink(Pool, Req) -> editChatInviteLink(Pool, Req, false).


-doc """
Use this method to create a subscription invite link for a channel chat.  
The bot must have the can_invite_users administrator rights.  
The link can be edited using the method editChatSubscriptionInviteLink or revoked using the method revokeChatInviteLink.  
Returns the new invite link as a ChatInviteLink object.
## Parameters
  * `chat_id` - Unique identifier for the target channel chat or username of the target channel (in the format @channelusername)
  * `name` - Invite link name; 0-32 characters
  * `subscription_period` - The number of seconds the subscription will be active for before the next payment. Currently, it must always be 2592000 (30 days).
  * `subscription_price` - The amount of Telegram Stars a user must pay initially and after each subsequent subscription period to be a member of the chat; 1-10000
""".
-doc (#{group=><<"Chat invite">>,since=><<"7.9">>}).
-spec createChatSubscriptionInviteLink(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), name => binary(), subscription_period := integer(), subscription_price := integer()}, Async :: boolean()) -> Result :: result('ChatInviteLink'()).
createChatSubscriptionInviteLink(Pool, #{chat_id:=_,subscription_period:=_,subscription_price:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"createChatSubscriptionInviteLink">>, Req, Async}).
-doc (#{equiv=>createChatSubscriptionInviteLink(Pool, Req, false),since=><<"7.9">>,group=><<"Sync Request">>}).
createChatSubscriptionInviteLink(Pool, Req) -> createChatSubscriptionInviteLink(Pool, Req, false).


-doc """
Use this method to edit a subscription invite link created by the bot.  
The bot must have the can_invite_users administrator rights.  
Returns the edited invite link as a ChatInviteLink object.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `invite_link` - The invite link to edit
  * `name` - Invite link name; 0-32 characters
""".
-doc (#{group=><<"Chat invite">>,since=><<"7.9">>}).
-spec editChatSubscriptionInviteLink(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), invite_link := binary(), name => binary()}, Async :: boolean()) -> Result :: result('ChatInviteLink'()).
editChatSubscriptionInviteLink(Pool, #{chat_id:=_,invite_link:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"editChatSubscriptionInviteLink">>, Req, Async}).
-doc (#{equiv=>editChatSubscriptionInviteLink(Pool, Req, false),since=><<"7.9">>,group=><<"Sync Request">>}).
editChatSubscriptionInviteLink(Pool, Req) -> editChatSubscriptionInviteLink(Pool, Req, false).


-doc """
Use this method to revoke an invite link created by the bot.  
If the primary link is revoked, a new link is automatically generated.  
The bot must be an administrator in the chat for this to work and must have the appropriate administrator rights.  
Returns the revoked invite link as ChatInviteLink object.
## Parameters
  * `chat_id` - Unique identifier of the target chat or username of the target channel (in the format @channelusername)
  * `invite_link` - The invite link to revoke
""".
-doc (#{group=><<"Chat invite">>,since=><<"5.1">>}).
-spec revokeChatInviteLink(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), invite_link := binary()}, Async :: boolean()) -> Result :: result('ChatInviteLink'()).
revokeChatInviteLink(Pool, #{chat_id:=_,invite_link:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"revokeChatInviteLink">>, Req, Async}).
-doc (#{equiv=>revokeChatInviteLink(Pool, Req, false),since=><<"5.1">>,group=><<"Sync Request">>}).
revokeChatInviteLink(Pool, Req) -> revokeChatInviteLink(Pool, Req, false).


-doc """
Use this method to approve a chat join request.  
The bot must be an administrator in the chat for this to work and must have the can_invite_users administrator right.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `user_id` - Unique identifier of the target user
""".
-doc (#{group=><<"Chat">>,since=><<"5.4">>}).
-spec approveChatJoinRequest(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), user_id := integer()}, Async :: boolean()) -> Result :: result(true).
approveChatJoinRequest(Pool, #{chat_id:=_,user_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"approveChatJoinRequest">>, Req, Async}).
-doc (#{equiv=>approveChatJoinRequest(Pool, Req, false),since=><<"5.4">>,group=><<"Sync Request">>}).
approveChatJoinRequest(Pool, Req) -> approveChatJoinRequest(Pool, Req, false).


-doc """
Use this method to decline a chat join request.  
The bot must be an administrator in the chat for this to work and must have the can_invite_users administrator right.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `user_id` - Unique identifier of the target user
""".
-doc (#{group=><<"Chat">>,since=><<"5.4">>}).
-spec declineChatJoinRequest(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), user_id := integer()}, Async :: boolean()) -> Result :: result(true).
declineChatJoinRequest(Pool, #{chat_id:=_,user_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"declineChatJoinRequest">>, Req, Async}).
-doc (#{equiv=>declineChatJoinRequest(Pool, Req, false),since=><<"5.4">>,group=><<"Sync Request">>}).
declineChatJoinRequest(Pool, Req) -> declineChatJoinRequest(Pool, Req, false).


-doc """
Use this method to set a new profile photo for the chat.  
Photos can't be changed for private chats.  
The bot must be an administrator in the chat for this to work and must have the appropriate administrator rights.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `photo` - New chat photo, uploaded using multipart/form-data
""".
-doc (#{group=><<"Chat">>,since=><<"3.1">>}).
-spec setChatPhoto(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), photo := 'InputFile'()}, Async :: boolean()) -> Result :: result(true).
setChatPhoto(Pool, #{chat_id:=_,photo:=_} = Req, Async) ->wpool:call(Pool, {multipart, <<"setChatPhoto">>, Req, Async}).
-doc (#{equiv=>setChatPhoto(Pool, Req, false),since=><<"3.1">>,group=><<"Sync Request">>}).
setChatPhoto(Pool, Req) -> setChatPhoto(Pool, Req, false).


-doc """
Use this method to delete a chat photo.  
Photos can't be changed for private chats.  
The bot must be an administrator in the chat for this to work and must have the appropriate administrator rights.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
""".
-doc (#{group=><<"Chat">>,since=><<"3.1">>}).
-spec deleteChatPhoto(Pool :: pool_name(), Req :: #{chat_id := integer() | binary()}, Async :: boolean()) -> Result :: result(true).
deleteChatPhoto(Pool, #{chat_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"deleteChatPhoto">>, Req, Async}).
-doc (#{equiv=>deleteChatPhoto(Pool, Req, false),since=><<"3.1">>,group=><<"Sync Request">>}).
deleteChatPhoto(Pool, Req) -> deleteChatPhoto(Pool, Req, false).


-doc """
Use this method to change the title of a chat.  
Titles can't be changed for private chats.  
The bot must be an administrator in the chat for this to work and must have the appropriate administrator rights.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `title` - New chat title, 1-128 characters
""".
-doc (#{group=><<"Chat">>,since=><<"3.1">>}).
-spec setChatTitle(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), title := binary()}, Async :: boolean()) -> Result :: result(true).
setChatTitle(Pool, #{chat_id:=_,title:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"setChatTitle">>, Req, Async}).
-doc (#{equiv=>setChatTitle(Pool, Req, false),since=><<"3.1">>,group=><<"Sync Request">>}).
setChatTitle(Pool, Req) -> setChatTitle(Pool, Req, false).


-doc """
Use this method to change the description of a group, a supergroup or a channel.  
The bot must be an administrator in the chat for this to work and must have the appropriate administrator rights.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `description` - New chat description, 0-255 characters
""".
-doc (#{group=><<"Chat">>,since=><<"3.1">>}).
-spec setChatDescription(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), description => binary()}, Async :: boolean()) -> Result :: result(true).
setChatDescription(Pool, #{chat_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"setChatDescription">>, Req, Async}).
-doc (#{equiv=>setChatDescription(Pool, Req, false),since=><<"3.1">>,group=><<"Sync Request">>}).
setChatDescription(Pool, Req) -> setChatDescription(Pool, Req, false).


-doc """
Use this method to add a message to the list of pinned messages in a chat.  
In private chats and channel direct messages chats, all non-service messages can be pinned.  
Conversely, the bot must be an administrator with the 'can_pin_messages' right or the 'can_edit_messages' right to pin messages in groups and channels respectively.  
Returns True on success.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the message will be pinned
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_id` - Identifier of a message to pin
  * `disable_notification` - Pass True if it is not necessary to send a notification to all chat members about the new pinned message. Notifications are always disabled in channels and private chats.
""".
-doc (#{group=><<"Message">>,since=><<"3.1">>}).
-spec pinChatMessage(Pool :: pool_name(), Req :: #{business_connection_id => binary(), chat_id := integer() | binary(), message_id := integer(), disable_notification => boolean()}, Async :: boolean()) -> Result :: result(true).
pinChatMessage(Pool, #{chat_id:=_,message_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"pinChatMessage">>, Req, Async}).
-doc (#{equiv=>pinChatMessage(Pool, Req, false),since=><<"3.1">>,group=><<"Sync Request">>}).
pinChatMessage(Pool, Req) -> pinChatMessage(Pool, Req, false).


-doc """
Use this method to remove a message from the list of pinned messages in a chat.  
In private chats and channel direct messages chats, all messages can be unpinned.  
Conversely, the bot must be an administrator with the 'can_pin_messages' right or the 'can_edit_messages' right to unpin messages in groups and channels respectively.  
Returns True on success.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the message will be unpinned
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_id` - Identifier of the message to unpin. Required if business_connection_id is specified. If not specified, the most recent pinned message (by sending date) will be unpinned.
""".
-doc (#{group=><<"Message">>,since=><<"3.1">>}).
-spec unpinChatMessage(Pool :: pool_name(), Req :: #{business_connection_id => binary(), chat_id := integer() | binary(), message_id => integer()}, Async :: boolean()) -> Result :: result(true).
unpinChatMessage(Pool, #{chat_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"unpinChatMessage">>, Req, Async}).
-doc (#{equiv=>unpinChatMessage(Pool, Req, false),since=><<"3.1">>,group=><<"Sync Request">>}).
unpinChatMessage(Pool, Req) -> unpinChatMessage(Pool, Req, false).


-doc """
Use this method to clear the list of pinned messages in a chat.  
In private chats and channel direct messages chats, no additional rights are required to unpin all pinned messages.  
Conversely, the bot must be an administrator with the 'can_pin_messages' right or the 'can_edit_messages' right to unpin all pinned messages in groups and channels respectively.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
""".
-doc (#{group=><<"Message">>,since=><<"5.0">>}).
-spec unpinAllChatMessages(Pool :: pool_name(), Req :: #{chat_id := integer() | binary()}, Async :: boolean()) -> Result :: result(true).
unpinAllChatMessages(Pool, #{chat_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"unpinAllChatMessages">>, Req, Async}).
-doc (#{equiv=>unpinAllChatMessages(Pool, Req, false),since=><<"5.0">>,group=><<"Sync Request">>}).
unpinAllChatMessages(Pool, Req) -> unpinAllChatMessages(Pool, Req, false).


-doc """
Use this method for your bot to leave a group, supergroup or channel.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target supergroup or channel (in the format @channelusername). Channel direct messages chats aren't supported; leave the corresponding channel instead.
""".
-doc (#{group=><<"Chat">>,since=><<"2.1">>}).
-spec leaveChat(Pool :: pool_name(), Req :: #{chat_id := integer() | binary()}, Async :: boolean()) -> Result :: result(true).
leaveChat(Pool, #{chat_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"leaveChat">>, Req, Async}).
-doc (#{equiv=>leaveChat(Pool, Req, false),since=><<"2.1">>,group=><<"Sync Request">>}).
leaveChat(Pool, Req) -> leaveChat(Pool, Req, false).


-doc """
Use this method to get up-to-date information about the chat.  
Returns a ChatFullInfo object on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target supergroup or channel (in the format @channelusername)
""".
-doc (#{group=><<"Chat">>,since=><<"2.1">>}).
-spec getChat(Pool :: pool_name(), Req :: #{chat_id := integer() | binary()}, Async :: boolean()) -> Result :: result('ChatFullInfo'()).
getChat(Pool, #{chat_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"getChat">>, Req, Async}).
-doc (#{equiv=>getChat(Pool, Req, false),since=><<"2.1">>,group=><<"Sync Request">>}).
getChat(Pool, Req) -> getChat(Pool, Req, false).


-doc """
Use this method to get a list of administrators in a chat, which aren't bots.  
Returns an Array of ChatMember objects.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target supergroup or channel (in the format @channelusername)
""".
-doc (#{group=><<"Chat">>,since=><<"2.1">>}).
-spec getChatAdministrators(Pool :: pool_name(), Req :: #{chat_id := integer() | binary()}, Async :: boolean()) -> Result :: result(nonempty_list('ChatMember'())).
getChatAdministrators(Pool, #{chat_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"getChatAdministrators">>, Req, Async}).
-doc (#{equiv=>getChatAdministrators(Pool, Req, false),since=><<"2.1">>,group=><<"Sync Request">>}).
getChatAdministrators(Pool, Req) -> getChatAdministrators(Pool, Req, false).


-doc """
Use this method to get the number of members in a chat.  
Returns Int on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target supergroup or channel (in the format @channelusername)
""".
-doc (#{group=><<"Chat">>,since=><<"5.3">>}).
-spec getChatMemberCount(Pool :: pool_name(), Req :: #{chat_id := integer() | binary()}, Async :: boolean()) -> Result :: result(integer()).
getChatMemberCount(Pool, #{chat_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"getChatMemberCount">>, Req, Async}).
-doc (#{equiv=>getChatMemberCount(Pool, Req, false),since=><<"5.3">>,group=><<"Sync Request">>}).
getChatMemberCount(Pool, Req) -> getChatMemberCount(Pool, Req, false).


-doc """
Use this method to get information about a member of a chat.  
The method is only guaranteed to work for other users if the bot is an administrator in the chat.  
Returns a ChatMember object on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target supergroup or channel (in the format @channelusername)
  * `user_id` - Unique identifier of the target user
""".
-doc (#{group=><<"Chat">>,since=><<"2.1">>}).
-spec getChatMember(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), user_id := integer()}, Async :: boolean()) -> Result :: result('ChatMember'()).
getChatMember(Pool, #{chat_id:=_,user_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"getChatMember">>, Req, Async}).
-doc (#{equiv=>getChatMember(Pool, Req, false),since=><<"2.1">>,group=><<"Sync Request">>}).
getChatMember(Pool, Req) -> getChatMember(Pool, Req, false).


-doc """
Use this method to set a new group sticker set for a supergroup.  
The bot must be an administrator in the chat for this to work and must have the appropriate administrator rights.  
Use the field can_set_sticker_set optionally returned in getChat requests to check if the bot can use this method.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  * `sticker_set_name` - Name of the sticker set to be set as the group sticker set
""".
-doc (#{group=><<"Sticker">>,since=><<"3.4">>}).
-spec setChatStickerSet(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), sticker_set_name := binary()}, Async :: boolean()) -> Result :: result(true).
setChatStickerSet(Pool, #{chat_id:=_,sticker_set_name:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"setChatStickerSet">>, Req, Async}).
-doc (#{equiv=>setChatStickerSet(Pool, Req, false),since=><<"3.4">>,group=><<"Sync Request">>}).
setChatStickerSet(Pool, Req) -> setChatStickerSet(Pool, Req, false).


-doc """
Use this method to delete a group sticker set from a supergroup.  
The bot must be an administrator in the chat for this to work and must have the appropriate administrator rights.  
Use the field can_set_sticker_set optionally returned in getChat requests to check if the bot can use this method.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
""".
-doc (#{group=><<"Sticker">>,since=><<"3.4">>}).
-spec deleteChatStickerSet(Pool :: pool_name(), Req :: #{chat_id := integer() | binary()}, Async :: boolean()) -> Result :: result(true).
deleteChatStickerSet(Pool, #{chat_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"deleteChatStickerSet">>, Req, Async}).
-doc (#{equiv=>deleteChatStickerSet(Pool, Req, false),since=><<"3.4">>,group=><<"Sync Request">>}).
deleteChatStickerSet(Pool, Req) -> deleteChatStickerSet(Pool, Req, false).


-doc """
Use this method to get custom emoji stickers, which can be used as a forum topic icon by any user.  
Requires no parameters.  
Returns an Array of Sticker objects.
""".
-doc (#{group=><<"Sticker">>,since=><<"6.3">>}).
-spec getForumTopicIconStickers(Pool :: pool_name(), Req :: empty_map(), Async :: boolean()) -> Result :: result(nonempty_list('Sticker'())).
getForumTopicIconStickers(Pool, #{} = Req, Async) ->wpool:call(Pool, {raw, <<"getForumTopicIconStickers">>, Req, Async}).
-doc (#{equiv=>getForumTopicIconStickers(Pool, Req, false),since=><<"6.3">>,group=><<"Sync Request">>}).
getForumTopicIconStickers(Pool, Req) -> getForumTopicIconStickers(Pool, Req, false).


-doc """
Use this method to create a topic in a forum supergroup chat.  
The bot must be an administrator in the chat for this to work and must have the can_manage_topics administrator rights.  
Returns information about the created topic as a ForumTopic object.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  * `name` - Topic name, 1-128 characters
  * `icon_color` - Color of the topic icon in RGB format. Currently, must be one of 7322096 (0x6FB9F0), 16766590 (0xFFD67E), 13338331 (0xCB86DB), 9367192 (0x8EEE98), 16749490 (0xFF93B2), or 16478047 (0xFB6F5F)
  * `icon_custom_emoji_id` - Unique identifier of the custom emoji shown as the topic icon. Use getForumTopicIconStickers to get all allowed custom emoji identifiers.
""".
-doc (#{group=><<"Forum">>,since=><<"6.3">>}).
-spec createForumTopic(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), name := binary(), icon_color => integer(), icon_custom_emoji_id => binary()}, Async :: boolean()) -> Result :: result('ForumTopic'()).
createForumTopic(Pool, #{chat_id:=_,name:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"createForumTopic">>, Req, Async}).
-doc (#{equiv=>createForumTopic(Pool, Req, false),since=><<"6.3">>,group=><<"Sync Request">>}).
createForumTopic(Pool, Req) -> createForumTopic(Pool, Req, false).


-doc """
Use this method to edit name and icon of a topic in a forum supergroup chat or a private chat with a user.  
In the case of a supergroup chat the bot must be an administrator in the chat for this to work and must have the can_manage_topics administrator rights, unless it is the creator of the topic.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  * `message_thread_id` - Unique identifier for the target message thread of the forum topic
  * `name` - New topic name, 0-128 characters. If not specified or empty, the current name of the topic will be kept
  * `icon_custom_emoji_id` - New unique identifier of the custom emoji shown as the topic icon. Use getForumTopicIconStickers to get all allowed custom emoji identifiers. Pass an empty string to remove the icon. If not specified, the current icon will be kept
""".
-doc (#{group=><<"Forum">>,since=><<"6.3">>}).
-spec editForumTopic(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), message_thread_id := integer(), name => binary(), icon_custom_emoji_id => binary()}, Async :: boolean()) -> Result :: result(true).
editForumTopic(Pool, #{chat_id:=_,message_thread_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"editForumTopic">>, Req, Async}).
-doc (#{equiv=>editForumTopic(Pool, Req, false),since=><<"6.3">>,group=><<"Sync Request">>}).
editForumTopic(Pool, Req) -> editForumTopic(Pool, Req, false).


-doc """
Use this method to close an open topic in a forum supergroup chat.  
The bot must be an administrator in the chat for this to work and must have the can_manage_topics administrator rights, unless it is the creator of the topic.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  * `message_thread_id` - Unique identifier for the target message thread of the forum topic
""".
-doc (#{group=><<"Forum">>,since=><<"6.3">>}).
-spec closeForumTopic(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), message_thread_id := integer()}, Async :: boolean()) -> Result :: result(true).
closeForumTopic(Pool, #{chat_id:=_,message_thread_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"closeForumTopic">>, Req, Async}).
-doc (#{equiv=>closeForumTopic(Pool, Req, false),since=><<"6.3">>,group=><<"Sync Request">>}).
closeForumTopic(Pool, Req) -> closeForumTopic(Pool, Req, false).


-doc """
Use this method to reopen a closed topic in a forum supergroup chat.  
The bot must be an administrator in the chat for this to work and must have the can_manage_topics administrator rights, unless it is the creator of the topic.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  * `message_thread_id` - Unique identifier for the target message thread of the forum topic
""".
-doc (#{group=><<"Forum">>,since=><<"6.3">>}).
-spec reopenForumTopic(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), message_thread_id := integer()}, Async :: boolean()) -> Result :: result(true).
reopenForumTopic(Pool, #{chat_id:=_,message_thread_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"reopenForumTopic">>, Req, Async}).
-doc (#{equiv=>reopenForumTopic(Pool, Req, false),since=><<"6.3">>,group=><<"Sync Request">>}).
reopenForumTopic(Pool, Req) -> reopenForumTopic(Pool, Req, false).


-doc """
Use this method to delete a forum topic along with all its messages in a forum supergroup chat or a private chat with a user.  
In the case of a supergroup chat the bot must be an administrator in the chat for this to work and must have the can_delete_messages administrator rights.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  * `message_thread_id` - Unique identifier for the target message thread of the forum topic
""".
-doc (#{group=><<"Forum">>,since=><<"6.3">>}).
-spec deleteForumTopic(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), message_thread_id := integer()}, Async :: boolean()) -> Result :: result(true).
deleteForumTopic(Pool, #{chat_id:=_,message_thread_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"deleteForumTopic">>, Req, Async}).
-doc (#{equiv=>deleteForumTopic(Pool, Req, false),since=><<"6.3">>,group=><<"Sync Request">>}).
deleteForumTopic(Pool, Req) -> deleteForumTopic(Pool, Req, false).


-doc """
Use this method to clear the list of pinned messages in a forum topic in a forum supergroup chat or a private chat with a user.  
In the case of a supergroup chat the bot must be an administrator in the chat for this to work and must have the can_pin_messages administrator right in the supergroup.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  * `message_thread_id` - Unique identifier for the target message thread of the forum topic
""".
-doc (#{group=><<"Forum">>,since=><<"6.3">>}).
-spec unpinAllForumTopicMessages(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), message_thread_id := integer()}, Async :: boolean()) -> Result :: result(true).
unpinAllForumTopicMessages(Pool, #{chat_id:=_,message_thread_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"unpinAllForumTopicMessages">>, Req, Async}).
-doc (#{equiv=>unpinAllForumTopicMessages(Pool, Req, false),since=><<"6.3">>,group=><<"Sync Request">>}).
unpinAllForumTopicMessages(Pool, Req) -> unpinAllForumTopicMessages(Pool, Req, false).


-doc """
Use this method to edit the name of the 'General' topic in a forum supergroup chat.  
The bot must be an administrator in the chat for this to work and must have the can_manage_topics administrator rights.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  * `name` - New topic name, 1-128 characters
""".
-doc (#{group=><<"Forum">>,since=><<"6.4">>}).
-spec editGeneralForumTopic(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), name := binary()}, Async :: boolean()) -> Result :: result(true).
editGeneralForumTopic(Pool, #{chat_id:=_,name:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"editGeneralForumTopic">>, Req, Async}).
-doc (#{equiv=>editGeneralForumTopic(Pool, Req, false),since=><<"6.4">>,group=><<"Sync Request">>}).
editGeneralForumTopic(Pool, Req) -> editGeneralForumTopic(Pool, Req, false).


-doc """
Use this method to close an open 'General' topic in a forum supergroup chat.  
The bot must be an administrator in the chat for this to work and must have the can_manage_topics administrator rights.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
""".
-doc (#{group=><<"Forum">>,since=><<"6.4">>}).
-spec closeGeneralForumTopic(Pool :: pool_name(), Req :: #{chat_id := integer() | binary()}, Async :: boolean()) -> Result :: result(true).
closeGeneralForumTopic(Pool, #{chat_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"closeGeneralForumTopic">>, Req, Async}).
-doc (#{equiv=>closeGeneralForumTopic(Pool, Req, false),since=><<"6.4">>,group=><<"Sync Request">>}).
closeGeneralForumTopic(Pool, Req) -> closeGeneralForumTopic(Pool, Req, false).


-doc """
Use this method to reopen a closed 'General' topic in a forum supergroup chat.  
The bot must be an administrator in the chat for this to work and must have the can_manage_topics administrator rights.  
The topic will be automatically unhidden if it was hidden.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
""".
-doc (#{group=><<"Forum">>,since=><<"6.4">>}).
-spec reopenGeneralForumTopic(Pool :: pool_name(), Req :: #{chat_id := integer() | binary()}, Async :: boolean()) -> Result :: result(true).
reopenGeneralForumTopic(Pool, #{chat_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"reopenGeneralForumTopic">>, Req, Async}).
-doc (#{equiv=>reopenGeneralForumTopic(Pool, Req, false),since=><<"6.4">>,group=><<"Sync Request">>}).
reopenGeneralForumTopic(Pool, Req) -> reopenGeneralForumTopic(Pool, Req, false).


-doc """
Use this method to hide the 'General' topic in a forum supergroup chat.  
The bot must be an administrator in the chat for this to work and must have the can_manage_topics administrator rights.  
The topic will be automatically closed if it was open.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
""".
-doc (#{group=><<"Forum">>,since=><<"6.4">>}).
-spec hideGeneralForumTopic(Pool :: pool_name(), Req :: #{chat_id := integer() | binary()}, Async :: boolean()) -> Result :: result(true).
hideGeneralForumTopic(Pool, #{chat_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"hideGeneralForumTopic">>, Req, Async}).
-doc (#{equiv=>hideGeneralForumTopic(Pool, Req, false),since=><<"6.4">>,group=><<"Sync Request">>}).
hideGeneralForumTopic(Pool, Req) -> hideGeneralForumTopic(Pool, Req, false).


-doc """
Use this method to unhide the 'General' topic in a forum supergroup chat.  
The bot must be an administrator in the chat for this to work and must have the can_manage_topics administrator rights.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
""".
-doc (#{group=><<"Forum">>,since=><<"6.4">>}).
-spec unhideGeneralForumTopic(Pool :: pool_name(), Req :: #{chat_id := integer() | binary()}, Async :: boolean()) -> Result :: result(true).
unhideGeneralForumTopic(Pool, #{chat_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"unhideGeneralForumTopic">>, Req, Async}).
-doc (#{equiv=>unhideGeneralForumTopic(Pool, Req, false),since=><<"6.4">>,group=><<"Sync Request">>}).
unhideGeneralForumTopic(Pool, Req) -> unhideGeneralForumTopic(Pool, Req, false).


-doc """
Use this method to clear the list of pinned messages in a General forum topic.  
The bot must be an administrator in the chat for this to work and must have the can_pin_messages administrator right in the supergroup.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
""".
-doc (#{group=><<"Forum">>,since=><<"6.8">>}).
-spec unpinAllGeneralForumTopicMessages(Pool :: pool_name(), Req :: #{chat_id := integer() | binary()}, Async :: boolean()) -> Result :: result(true).
unpinAllGeneralForumTopicMessages(Pool, #{chat_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"unpinAllGeneralForumTopicMessages">>, Req, Async}).
-doc (#{equiv=>unpinAllGeneralForumTopicMessages(Pool, Req, false),since=><<"6.8">>,group=><<"Sync Request">>}).
unpinAllGeneralForumTopicMessages(Pool, Req) -> unpinAllGeneralForumTopicMessages(Pool, Req, false).


-doc """
Use this method to send answers to callback queries sent from inline keyboards.  
The answer will be displayed to the user as a notification at the top of the chat screen or as an alert.  
On success, True is returned.  
Alternatively, the user can be redirected to the specified Game URL.  
For this option to work, you must first create a game for your bot via @BotFather and accept the terms.  
Otherwise, you may use links like t.me/your_bot?start=XXXX that open your bot with a parameter.
## Parameters
  * `callback_query_id` - Unique identifier for the query to be answered
  * `text` - Text of the notification. If not specified, nothing will be shown to the user, 0-200 characters
  * `show_alert` - If True, an alert will be shown by the client instead of a notification at the top of the chat screen. Defaults to false.
  * `url` - URL that will be opened by the user's client. If you have created a Game and accepted the conditions via @BotFather, specify the URL that opens your game - note that this will only work if the query comes from a callback_game button.

Otherwise, you may use links like t.me/your_bot?start=XXXX that open your bot with a parameter.
  * `cache_time` - The maximum amount of time in seconds that the result of the callback query may be cached client-side. Telegram apps will support caching starting in version 3.14. Defaults to 0.
""".
-doc (#{group=><<"Inline Mode">>,since=><<"2.0">>}).
-spec answerCallbackQuery(Pool :: pool_name(), Req :: #{callback_query_id := binary(), text => binary(), show_alert => boolean(), url => binary(), cache_time => integer()}, Async :: boolean()) -> Result :: result(true).
answerCallbackQuery(Pool, #{callback_query_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"answerCallbackQuery">>, Req, Async}).
-doc (#{equiv=>answerCallbackQuery(Pool, Req, false),since=><<"2.0">>,group=><<"Sync Request">>}).
answerCallbackQuery(Pool, Req) -> answerCallbackQuery(Pool, Req, false).


-doc """
Use this method to get the list of boosts added to a chat by a user.  
Requires administrator rights in the chat.  
Returns a UserChatBoosts object.
## Parameters
  * `chat_id` - Unique identifier for the chat or username of the channel (in the format @channelusername)
  * `user_id` - Unique identifier of the target user
""".
-doc (#{group=><<"Chat">>,since=><<"7.0">>}).
-spec getUserChatBoosts(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), user_id := integer()}, Async :: boolean()) -> Result :: result('UserChatBoosts'()).
getUserChatBoosts(Pool, #{chat_id:=_,user_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"getUserChatBoosts">>, Req, Async}).
-doc (#{equiv=>getUserChatBoosts(Pool, Req, false),since=><<"7.0">>,group=><<"Sync Request">>}).
getUserChatBoosts(Pool, Req) -> getUserChatBoosts(Pool, Req, false).


-doc """
Use this method to get information about the connection of the bot with a business account.  
Returns a BusinessConnection object on success.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection
""".
-doc (#{group=><<"Business Account">>,since=><<"7.2">>}).
-spec getBusinessConnection(Pool :: pool_name(), Req :: #{business_connection_id := binary()}, Async :: boolean()) -> Result :: result('BusinessConnection'()).
getBusinessConnection(Pool, #{business_connection_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"getBusinessConnection">>, Req, Async}).
-doc (#{equiv=>getBusinessConnection(Pool, Req, false),since=><<"7.2">>,group=><<"Sync Request">>}).
getBusinessConnection(Pool, Req) -> getBusinessConnection(Pool, Req, false).


-doc """
Use this method to change the list of the bot's commands.  
See this manual for more details about bot commands.  
Returns True on success.
## Parameters
  * `commands` - A JSON-serialized list of bot commands to be set as the list of the bot's commands. At most 100 commands can be specified.
  * `scope` - A JSON-serialized object, describing scope of users for which the commands are relevant. Defaults to BotCommandScopeDefault.
  * `language_code` - A two-letter ISO 639-1 language code. If empty, commands will be applied to all users from the given scope, for whose language there are no dedicated commands
""".
-doc (#{group=><<"Bot Settings">>,since=><<"4.7">>}).
-spec setMyCommands(Pool :: pool_name(), Req :: #{commands := nonempty_list('BotCommand'()), scope => 'BotCommandScope'(), language_code => binary()}, Async :: boolean()) -> Result :: result(true).
setMyCommands(Pool, #{commands:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"setMyCommands">>, Req, Async}).
-doc (#{equiv=>setMyCommands(Pool, Req, false),since=><<"4.7">>,group=><<"Sync Request">>}).
setMyCommands(Pool, Req) -> setMyCommands(Pool, Req, false).


-doc """
Use this method to delete the list of the bot's commands for the given scope and user language.  
After deletion, higher level commands will be shown to affected users.  
Returns True on success.
## Parameters
  * `scope` - A JSON-serialized object, describing scope of users for which the commands are relevant. Defaults to BotCommandScopeDefault.
  * `language_code` - A two-letter ISO 639-1 language code. If empty, commands will be applied to all users from the given scope, for whose language there are no dedicated commands
""".
-doc (#{group=><<"Bot Settings">>,since=><<"5.3">>}).
-spec deleteMyCommands(Pool :: pool_name(), Req :: #{scope => 'BotCommandScope'(), language_code => binary()}, Async :: boolean()) -> Result :: result(true).
deleteMyCommands(Pool, #{} = Req, Async) ->wpool:call(Pool, {raw, <<"deleteMyCommands">>, Req, Async}).
-doc (#{equiv=>deleteMyCommands(Pool, Req, false),since=><<"5.3">>,group=><<"Sync Request">>}).
deleteMyCommands(Pool, Req) -> deleteMyCommands(Pool, Req, false).


-doc """
Use this method to get the current list of the bot's commands for the given scope and user language.  
Returns an Array of BotCommand objects.  
If commands aren't set, an empty list is returned.
## Parameters
  * `scope` - A JSON-serialized object, describing scope of users. Defaults to BotCommandScopeDefault.
  * `language_code` - A two-letter ISO 639-1 language code or an empty string
""".
-doc (#{group=><<"Bot Settings">>,since=><<"4.7">>}).
-spec getMyCommands(Pool :: pool_name(), Req :: #{scope => 'BotCommandScope'(), language_code => binary()}, Async :: boolean()) -> Result :: result(nonempty_list('BotCommand'())).
getMyCommands(Pool, #{} = Req, Async) ->wpool:call(Pool, {raw, <<"getMyCommands">>, Req, Async}).
-doc (#{equiv=>getMyCommands(Pool, Req, false),since=><<"4.7">>,group=><<"Sync Request">>}).
getMyCommands(Pool, Req) -> getMyCommands(Pool, Req, false).


-doc """
Use this method to change the bot's name.  
Returns True on success.
## Parameters
  * `name` - New bot name; 0-64 characters. Pass an empty string to remove the dedicated name for the given language.
  * `language_code` - A two-letter ISO 639-1 language code. If empty, the name will be shown to all users for whose language there is no dedicated name.
""".
-doc (#{group=><<"Bot Settings">>,since=><<"6.7">>}).
-spec setMyName(Pool :: pool_name(), Req :: #{name => binary(), language_code => binary()}, Async :: boolean()) -> Result :: result(true).
setMyName(Pool, #{} = Req, Async) ->wpool:call(Pool, {raw, <<"setMyName">>, Req, Async}).
-doc (#{equiv=>setMyName(Pool, Req, false),since=><<"6.7">>,group=><<"Sync Request">>}).
setMyName(Pool, Req) -> setMyName(Pool, Req, false).


-doc """
Use this method to get the current bot name for the given user language.  
Returns BotName on success.
## Parameters
  * `language_code` - A two-letter ISO 639-1 language code or an empty string
""".
-doc (#{group=><<"Bot Settings">>,since=><<"6.7">>}).
-spec getMyName(Pool :: pool_name(), Req :: #{language_code => binary()}, Async :: boolean()) -> Result :: result('BotName'()).
getMyName(Pool, #{} = Req, Async) ->wpool:call(Pool, {raw, <<"getMyName">>, Req, Async}).
-doc (#{equiv=>getMyName(Pool, Req, false),since=><<"6.7">>,group=><<"Sync Request">>}).
getMyName(Pool, Req) -> getMyName(Pool, Req, false).


-doc """
Use this method to change the bot's description, which is shown in the chat with the bot if the chat is empty.  
Returns True on success.
## Parameters
  * `description` - New bot description; 0-512 characters. Pass an empty string to remove the dedicated description for the given language.
  * `language_code` - A two-letter ISO 639-1 language code. If empty, the description will be applied to all users for whose language there is no dedicated description.
""".
-doc (#{group=><<"Bot Settings">>,since=><<"6.6">>}).
-spec setMyDescription(Pool :: pool_name(), Req :: #{description => binary(), language_code => binary()}, Async :: boolean()) -> Result :: result(true).
setMyDescription(Pool, #{} = Req, Async) ->wpool:call(Pool, {raw, <<"setMyDescription">>, Req, Async}).
-doc (#{equiv=>setMyDescription(Pool, Req, false),since=><<"6.6">>,group=><<"Sync Request">>}).
setMyDescription(Pool, Req) -> setMyDescription(Pool, Req, false).


-doc """
Use this method to get the current bot description for the given user language.  
Returns BotDescription on success.
## Parameters
  * `language_code` - A two-letter ISO 639-1 language code or an empty string
""".
-doc (#{group=><<"Bot Settings">>,since=><<"6.6">>}).
-spec getMyDescription(Pool :: pool_name(), Req :: #{language_code => binary()}, Async :: boolean()) -> Result :: result('BotDescription'()).
getMyDescription(Pool, #{} = Req, Async) ->wpool:call(Pool, {raw, <<"getMyDescription">>, Req, Async}).
-doc (#{equiv=>getMyDescription(Pool, Req, false),since=><<"6.6">>,group=><<"Sync Request">>}).
getMyDescription(Pool, Req) -> getMyDescription(Pool, Req, false).


-doc """
Use this method to change the bot's short description, which is shown on the bot's profile page and is sent together with the link when users share the bot.  
Returns True on success.
## Parameters
  * `short_description` - New short description for the bot; 0-120 characters. Pass an empty string to remove the dedicated short description for the given language.
  * `language_code` - A two-letter ISO 639-1 language code. If empty, the short description will be applied to all users for whose language there is no dedicated short description.
""".
-doc (#{group=><<"Bot Settings">>,since=><<"6.6">>}).
-spec setMyShortDescription(Pool :: pool_name(), Req :: #{short_description => binary(), language_code => binary()}, Async :: boolean()) -> Result :: result(true).
setMyShortDescription(Pool, #{} = Req, Async) ->wpool:call(Pool, {raw, <<"setMyShortDescription">>, Req, Async}).
-doc (#{equiv=>setMyShortDescription(Pool, Req, false),since=><<"6.6">>,group=><<"Sync Request">>}).
setMyShortDescription(Pool, Req) -> setMyShortDescription(Pool, Req, false).


-doc """
Use this method to get the current bot short description for the given user language.  
Returns BotShortDescription on success.
## Parameters
  * `language_code` - A two-letter ISO 639-1 language code or an empty string
""".
-doc (#{group=><<"Bot Settings">>,since=><<"6.6">>}).
-spec getMyShortDescription(Pool :: pool_name(), Req :: #{language_code => binary()}, Async :: boolean()) -> Result :: result('BotShortDescription'()).
getMyShortDescription(Pool, #{} = Req, Async) ->wpool:call(Pool, {raw, <<"getMyShortDescription">>, Req, Async}).
-doc (#{equiv=>getMyShortDescription(Pool, Req, false),since=><<"6.6">>,group=><<"Sync Request">>}).
getMyShortDescription(Pool, Req) -> getMyShortDescription(Pool, Req, false).


-doc """
Use this method to change the bot's menu button in a private chat, or the default menu button.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target private chat. If not specified, default bot's menu button will be changed
  * `menu_button` - A JSON-serialized object for the bot's new menu button. Defaults to MenuButtonDefault
""".
-doc (#{group=><<"Bot Settings">>,since=><<"6.0">>}).
-spec setChatMenuButton(Pool :: pool_name(), Req :: #{chat_id => integer(), menu_button => 'MenuButton'()}, Async :: boolean()) -> Result :: result(true).
setChatMenuButton(Pool, #{} = Req, Async) ->wpool:call(Pool, {raw, <<"setChatMenuButton">>, Req, Async}).
-doc (#{equiv=>setChatMenuButton(Pool, Req, false),since=><<"6.0">>,group=><<"Sync Request">>}).
setChatMenuButton(Pool, Req) -> setChatMenuButton(Pool, Req, false).


-doc """
Use this method to get the current value of the bot's menu button in a private chat, or the default menu button.  
Returns MenuButton on success.
## Parameters
  * `chat_id` - Unique identifier for the target private chat. If not specified, default bot's menu button will be returned
""".
-doc (#{group=><<"Bot Settings">>,since=><<"6.0">>}).
-spec getChatMenuButton(Pool :: pool_name(), Req :: #{chat_id => integer()}, Async :: boolean()) -> Result :: result('MenuButton'()).
getChatMenuButton(Pool, #{} = Req, Async) ->wpool:call(Pool, {raw, <<"getChatMenuButton">>, Req, Async}).
-doc (#{equiv=>getChatMenuButton(Pool, Req, false),since=><<"6.0">>,group=><<"Sync Request">>}).
getChatMenuButton(Pool, Req) -> getChatMenuButton(Pool, Req, false).


-doc """
Use this method to change the default administrator rights requested by the bot when it's added as an administrator to groups or channels.  
These rights will be suggested to users, but they are free to modify the list before adding the bot.  
Returns True on success.
## Parameters
  * `rights` - A JSON-serialized object describing new default administrator rights. If not specified, the default administrator rights will be cleared.
  * `for_channels` - Pass True to change the default administrator rights of the bot in channels. Otherwise, the default administrator rights of the bot for groups and supergroups will be changed.
""".
-doc (#{group=><<"Bot Settings">>,since=><<"6.0">>}).
-spec setMyDefaultAdministratorRights(Pool :: pool_name(), Req :: #{rights => 'ChatAdministratorRights'(), for_channels => boolean()}, Async :: boolean()) -> Result :: result(true).
setMyDefaultAdministratorRights(Pool, #{} = Req, Async) ->wpool:call(Pool, {raw, <<"setMyDefaultAdministratorRights">>, Req, Async}).
-doc (#{equiv=>setMyDefaultAdministratorRights(Pool, Req, false),since=><<"6.0">>,group=><<"Sync Request">>}).
setMyDefaultAdministratorRights(Pool, Req) -> setMyDefaultAdministratorRights(Pool, Req, false).


-doc """
Use this method to get the current default administrator rights of the bot.  
Returns ChatAdministratorRights on success.
## Parameters
  * `for_channels` - Pass True to get default administrator rights of the bot in channels. Otherwise, default administrator rights of the bot for groups and supergroups will be returned.
""".
-doc (#{group=><<"Bot Settings">>,since=><<"6.0">>}).
-spec getMyDefaultAdministratorRights(Pool :: pool_name(), Req :: #{for_channels => boolean()}, Async :: boolean()) -> Result :: result('ChatAdministratorRights'()).
getMyDefaultAdministratorRights(Pool, #{} = Req, Async) ->wpool:call(Pool, {raw, <<"getMyDefaultAdministratorRights">>, Req, Async}).
-doc (#{equiv=>getMyDefaultAdministratorRights(Pool, Req, false),since=><<"6.0">>,group=><<"Sync Request">>}).
getMyDefaultAdministratorRights(Pool, Req) -> getMyDefaultAdministratorRights(Pool, Req, false).


-doc """
Returns the list of gifts that can be sent by the bot to users and channel chats.  
Requires no parameters.  
Returns a Gifts object.
""".
-doc (#{group=><<"Gift">>,since=><<"8.0">>}).
-spec getAvailableGifts(Pool :: pool_name(), Req :: empty_map(), Async :: boolean()) -> Result :: result('Gifts'()).
getAvailableGifts(Pool, #{} = Req, Async) ->wpool:call(Pool, {raw, <<"getAvailableGifts">>, Req, Async}).
-doc (#{equiv=>getAvailableGifts(Pool, Req, false),since=><<"8.0">>,group=><<"Sync Request">>}).
getAvailableGifts(Pool, Req) -> getAvailableGifts(Pool, Req, false).


-doc """
Sends a gift to the given user or channel chat.  
The gift can't be converted to Telegram Stars by the receiver.  
Returns True on success.
## Parameters
  * `user_id` - Required if chat_id is not specified. Unique identifier of the target user who will receive the gift.
  * `chat_id` - Required if user_id is not specified. Unique identifier for the chat or username of the channel (in the format @channelusername) that will receive the gift.
  * `gift_id` - Identifier of the gift; limited gifts can't be sent to channel chats
  * `pay_for_upgrade` - Pass True to pay for the gift upgrade from the bot's balance, thereby making the upgrade free for the receiver
  * `text` - Text that will be shown along with the gift; 0-128 characters
  * `text_parse_mode` - Mode for parsing entities in the text. See formatting options for more details. Entities other than “bold”, “italic”, “underline”, “strikethrough”, “spoiler”, and “custom_emoji” are ignored.
  * `text_entities` - A JSON-serialized list of special entities that appear in the gift text. It can be specified instead of text_parse_mode. Entities other than “bold”, “italic”, “underline”, “strikethrough”, “spoiler”, and “custom_emoji” are ignored.
""".
-doc (#{group=><<"Gift">>,since=><<"8.0">>}).
-spec sendGift(Pool :: pool_name(), Req :: #{user_id => integer(), chat_id => integer() | binary(), gift_id := binary(), pay_for_upgrade => boolean(), text => binary(), text_parse_mode => binary(), text_entities => nonempty_list('MessageEntity'())}, Async :: boolean()) -> Result :: result(true).
sendGift(Pool, #{gift_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"sendGift">>, Req, Async}).
-doc (#{equiv=>sendGift(Pool, Req, false),since=><<"8.0">>,group=><<"Sync Request">>}).
sendGift(Pool, Req) -> sendGift(Pool, Req, false).


-doc """
Gifts a Telegram Premium subscription to the given user.  
Returns True on success.
## Parameters
  * `user_id` - Unique identifier of the target user who will receive a Telegram Premium subscription
  * `month_count` - Number of months the Telegram Premium subscription will be active for the user; must be one of 3, 6, or 12
  * `star_count` - Number of Telegram Stars to pay for the Telegram Premium subscription; must be 1000 for 3 months, 1500 for 6 months, and 2500 for 12 months
  * `text` - Text that will be shown along with the service message about the subscription; 0-128 characters
  * `text_parse_mode` - Mode for parsing entities in the text. See formatting options for more details. Entities other than “bold”, “italic”, “underline”, “strikethrough”, “spoiler”, and “custom_emoji” are ignored.
  * `text_entities` - A JSON-serialized list of special entities that appear in the gift text. It can be specified instead of text_parse_mode. Entities other than “bold”, “italic”, “underline”, “strikethrough”, “spoiler”, and “custom_emoji” are ignored.
""".
-doc (#{group=><<"Gift">>,since=><<"9.0">>}).
-spec giftPremiumSubscription(Pool :: pool_name(), Req :: #{user_id := integer(), month_count := integer(), star_count := integer(), text => binary(), text_parse_mode => binary(), text_entities => nonempty_list('MessageEntity'())}, Async :: boolean()) -> Result :: result(true).
giftPremiumSubscription(Pool, #{user_id:=_,month_count:=_,star_count:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"giftPremiumSubscription">>, Req, Async}).
-doc (#{equiv=>giftPremiumSubscription(Pool, Req, false),since=><<"9.0">>,group=><<"Sync Request">>}).
giftPremiumSubscription(Pool, Req) -> giftPremiumSubscription(Pool, Req, false).


-doc """
Verifies a user on behalf of the organization which is represented by the bot.  
Returns True on success.
## Parameters
  * `user_id` - Unique identifier of the target user
  * `custom_description` - Custom description for the verification; 0-70 characters. Must be empty if the organization isn't allowed to provide a custom verification description.
""".
-doc (#{group=><<"Verification">>,since=><<"8.2">>}).
-spec verifyUser(Pool :: pool_name(), Req :: #{user_id := integer(), custom_description => binary()}, Async :: boolean()) -> Result :: result(true).
verifyUser(Pool, #{user_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"verifyUser">>, Req, Async}).
-doc (#{equiv=>verifyUser(Pool, Req, false),since=><<"8.2">>,group=><<"Sync Request">>}).
verifyUser(Pool, Req) -> verifyUser(Pool, Req, false).


-doc """
Verifies a chat on behalf of the organization which is represented by the bot.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername). Channel direct messages chats can't be verified.
  * `custom_description` - Custom description for the verification; 0-70 characters. Must be empty if the organization isn't allowed to provide a custom verification description.
""".
-doc (#{group=><<"Verification">>,since=><<"8.2">>}).
-spec verifyChat(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), custom_description => binary()}, Async :: boolean()) -> Result :: result(true).
verifyChat(Pool, #{chat_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"verifyChat">>, Req, Async}).
-doc (#{equiv=>verifyChat(Pool, Req, false),since=><<"8.2">>,group=><<"Sync Request">>}).
verifyChat(Pool, Req) -> verifyChat(Pool, Req, false).


-doc """
Removes verification from a user who is currently verified on behalf of the organization represented by the bot.  
Returns True on success.
## Parameters
  * `user_id` - Unique identifier of the target user
""".
-doc (#{group=><<"Verification">>,since=><<"8.2">>}).
-spec removeUserVerification(Pool :: pool_name(), Req :: #{user_id := integer()}, Async :: boolean()) -> Result :: result(true).
removeUserVerification(Pool, #{user_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"removeUserVerification">>, Req, Async}).
-doc (#{equiv=>removeUserVerification(Pool, Req, false),since=><<"8.2">>,group=><<"Sync Request">>}).
removeUserVerification(Pool, Req) -> removeUserVerification(Pool, Req, false).


-doc """
Removes verification from a chat that is currently verified on behalf of the organization represented by the bot.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
""".
-doc (#{group=><<"Verification">>,since=><<"8.2">>}).
-spec removeChatVerification(Pool :: pool_name(), Req :: #{chat_id := integer() | binary()}, Async :: boolean()) -> Result :: result(true).
removeChatVerification(Pool, #{chat_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"removeChatVerification">>, Req, Async}).
-doc (#{equiv=>removeChatVerification(Pool, Req, false),since=><<"8.2">>,group=><<"Sync Request">>}).
removeChatVerification(Pool, Req) -> removeChatVerification(Pool, Req, false).


-doc """
Marks incoming message as read on behalf of a business account.  
Requires the can_read_messages business bot right.  
Returns True on success.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which to read the message
  * `chat_id` - Unique identifier of the chat in which the message was received. The chat must have been active in the last 24 hours.
  * `message_id` - Unique identifier of the message to mark as read
""".
-doc (#{group=><<"Message">>,since=><<"9.0">>}).
-spec readBusinessMessage(Pool :: pool_name(), Req :: #{business_connection_id := binary(), chat_id := integer(), message_id := integer()}, Async :: boolean()) -> Result :: result(true).
readBusinessMessage(Pool, #{business_connection_id:=_,chat_id:=_,message_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"readBusinessMessage">>, Req, Async}).
-doc (#{equiv=>readBusinessMessage(Pool, Req, false),since=><<"9.0">>,group=><<"Sync Request">>}).
readBusinessMessage(Pool, Req) -> readBusinessMessage(Pool, Req, false).


-doc """
Delete messages on behalf of a business account.  
Requires the can_delete_sent_messages business bot right to delete messages sent by the bot itself, or the can_delete_all_messages business bot right to delete any message.  
Returns True on success.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which to delete the messages
  * `message_ids` - A JSON-serialized list of 1-100 identifiers of messages to delete. All messages must be from the same chat. See deleteMessage for limitations on which messages can be deleted
""".
-doc (#{group=><<"Message">>,since=><<"9.0">>}).
-spec deleteBusinessMessages(Pool :: pool_name(), Req :: #{business_connection_id := binary(), message_ids := nonempty_list(integer())}, Async :: boolean()) -> Result :: result(true).
deleteBusinessMessages(Pool, #{business_connection_id:=_,message_ids:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"deleteBusinessMessages">>, Req, Async}).
-doc (#{equiv=>deleteBusinessMessages(Pool, Req, false),since=><<"9.0">>,group=><<"Sync Request">>}).
deleteBusinessMessages(Pool, Req) -> deleteBusinessMessages(Pool, Req, false).


-doc """
Changes the first and last name of a managed business account.  
Requires the can_change_name business bot right.  
Returns True on success.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection
  * `first_name` - The new value of the first name for the business account; 1-64 characters
  * `last_name` - The new value of the last name for the business account; 0-64 characters
""".
-doc (#{group=><<"Business Account">>,since=><<"9.0">>}).
-spec setBusinessAccountName(Pool :: pool_name(), Req :: #{business_connection_id := binary(), first_name := binary(), last_name => binary()}, Async :: boolean()) -> Result :: result(true).
setBusinessAccountName(Pool, #{business_connection_id:=_,first_name:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"setBusinessAccountName">>, Req, Async}).
-doc (#{equiv=>setBusinessAccountName(Pool, Req, false),since=><<"9.0">>,group=><<"Sync Request">>}).
setBusinessAccountName(Pool, Req) -> setBusinessAccountName(Pool, Req, false).


-doc """
Changes the username of a managed business account.  
Requires the can_change_username business bot right.  
Returns True on success.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection
  * `username` - The new value of the username for the business account; 0-32 characters
""".
-doc (#{group=><<"Business Account">>,since=><<"9.0">>}).
-spec setBusinessAccountUsername(Pool :: pool_name(), Req :: #{business_connection_id := binary(), username => binary()}, Async :: boolean()) -> Result :: result(true).
setBusinessAccountUsername(Pool, #{business_connection_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"setBusinessAccountUsername">>, Req, Async}).
-doc (#{equiv=>setBusinessAccountUsername(Pool, Req, false),since=><<"9.0">>,group=><<"Sync Request">>}).
setBusinessAccountUsername(Pool, Req) -> setBusinessAccountUsername(Pool, Req, false).


-doc """
Changes the bio of a managed business account.  
Requires the can_change_bio business bot right.  
Returns True on success.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection
  * `bio` - The new value of the bio for the business account; 0-140 characters
""".
-doc (#{group=><<"Business Account">>,since=><<"9.0">>}).
-spec setBusinessAccountBio(Pool :: pool_name(), Req :: #{business_connection_id := binary(), bio => binary()}, Async :: boolean()) -> Result :: result(true).
setBusinessAccountBio(Pool, #{business_connection_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"setBusinessAccountBio">>, Req, Async}).
-doc (#{equiv=>setBusinessAccountBio(Pool, Req, false),since=><<"9.0">>,group=><<"Sync Request">>}).
setBusinessAccountBio(Pool, Req) -> setBusinessAccountBio(Pool, Req, false).


-doc """
Changes the profile photo of a managed business account.  
Requires the can_edit_profile_photo business bot right.  
Returns True on success.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection
  * `photo` - The new profile photo to set
  * `is_public` - Pass True to set the public photo, which will be visible even if the main photo is hidden by the business account's privacy settings. An account can have only one public photo.
""".
-doc (#{group=><<"Business Account">>,since=><<"9.0">>}).
-spec setBusinessAccountProfilePhoto(Pool :: pool_name(), Req :: #{business_connection_id := binary(), photo := 'InputProfilePhoto'(), is_public => boolean()}, Async :: boolean()) -> Result :: result(true).
setBusinessAccountProfilePhoto(Pool, #{business_connection_id:=_,photo:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"setBusinessAccountProfilePhoto">>, Req, Async}).
-doc (#{equiv=>setBusinessAccountProfilePhoto(Pool, Req, false),since=><<"9.0">>,group=><<"Sync Request">>}).
setBusinessAccountProfilePhoto(Pool, Req) -> setBusinessAccountProfilePhoto(Pool, Req, false).


-doc """
Removes the current profile photo of a managed business account.  
Requires the can_edit_profile_photo business bot right.  
Returns True on success.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection
  * `is_public` - Pass True to remove the public photo, which is visible even if the main photo is hidden by the business account's privacy settings. After the main photo is removed, the previous profile photo (if present) becomes the main photo.
""".
-doc (#{group=><<"Business Account">>,since=><<"9.0">>}).
-spec removeBusinessAccountProfilePhoto(Pool :: pool_name(), Req :: #{business_connection_id := binary(), is_public => boolean()}, Async :: boolean()) -> Result :: result(true).
removeBusinessAccountProfilePhoto(Pool, #{business_connection_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"removeBusinessAccountProfilePhoto">>, Req, Async}).
-doc (#{equiv=>removeBusinessAccountProfilePhoto(Pool, Req, false),since=><<"9.0">>,group=><<"Sync Request">>}).
removeBusinessAccountProfilePhoto(Pool, Req) -> removeBusinessAccountProfilePhoto(Pool, Req, false).


-doc """
Changes the privacy settings pertaining to incoming gifts in a managed business account.  
Requires the can_change_gift_settings business bot right.  
Returns True on success.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection
  * `show_gift_button` - Pass True, if a button for sending a gift to the user or by the business account must always be shown in the input field
  * `accepted_gift_types` - Types of gifts accepted by the business account
""".
-doc (#{group=><<"Gift">>,since=><<"9.0">>}).
-spec setBusinessAccountGiftSettings(Pool :: pool_name(), Req :: #{business_connection_id := binary(), show_gift_button := boolean(), accepted_gift_types := 'AcceptedGiftTypes'()}, Async :: boolean()) -> Result :: result(true).
setBusinessAccountGiftSettings(Pool, #{business_connection_id:=_,show_gift_button:=_,accepted_gift_types:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"setBusinessAccountGiftSettings">>, Req, Async}).
-doc (#{equiv=>setBusinessAccountGiftSettings(Pool, Req, false),since=><<"9.0">>,group=><<"Sync Request">>}).
setBusinessAccountGiftSettings(Pool, Req) -> setBusinessAccountGiftSettings(Pool, Req, false).


-doc """
Returns the amount of Telegram Stars owned by a managed business account.  
Requires the can_view_gifts_and_stars business bot right.  
Returns StarAmount on success.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection
""".
-doc (#{group=><<"Payments">>,since=><<"9.0">>}).
-spec getBusinessAccountStarBalance(Pool :: pool_name(), Req :: #{business_connection_id := binary()}, Async :: boolean()) -> Result :: result('StarAmount'()).
getBusinessAccountStarBalance(Pool, #{business_connection_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"getBusinessAccountStarBalance">>, Req, Async}).
-doc (#{equiv=>getBusinessAccountStarBalance(Pool, Req, false),since=><<"9.0">>,group=><<"Sync Request">>}).
getBusinessAccountStarBalance(Pool, Req) -> getBusinessAccountStarBalance(Pool, Req, false).


-doc """
Transfers Telegram Stars from the business account balance to the bot's balance.  
Requires the can_transfer_stars business bot right.  
Returns True on success.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection
  * `star_count` - Number of Telegram Stars to transfer; 1-10000
""".
-doc (#{group=><<"Payments">>,since=><<"9.0">>}).
-spec transferBusinessAccountStars(Pool :: pool_name(), Req :: #{business_connection_id := binary(), star_count := integer()}, Async :: boolean()) -> Result :: result(true).
transferBusinessAccountStars(Pool, #{business_connection_id:=_,star_count:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"transferBusinessAccountStars">>, Req, Async}).
-doc (#{equiv=>transferBusinessAccountStars(Pool, Req, false),since=><<"9.0">>,group=><<"Sync Request">>}).
transferBusinessAccountStars(Pool, Req) -> transferBusinessAccountStars(Pool, Req, false).


-doc """
Returns the gifts received and owned by a managed business account.  
Requires the can_view_gifts_and_stars business bot right.  
Returns OwnedGifts on success.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection
  * `exclude_unsaved` - Pass True to exclude gifts that aren't saved to the account's profile page
  * `exclude_saved` - Pass True to exclude gifts that are saved to the account's profile page
  * `exclude_unlimited` - Pass True to exclude gifts that can be purchased an unlimited number of times
  * `exclude_limited_upgradable` - Pass True to exclude gifts that can be purchased a limited number of times and can be upgraded to unique
  * `exclude_limited_non_upgradable` - Pass True to exclude gifts that can be purchased a limited number of times and can't be upgraded to unique
  * `exclude_unique` - Pass True to exclude unique gifts
  * `exclude_from_blockchain` - Pass True to exclude gifts that were assigned from the TON blockchain and can't be resold or transferred in Telegram
  * `sort_by_price` - Pass True to sort results by gift price instead of send date. Sorting is applied before pagination.
  * `offset` - Offset of the first entry to return as received from the previous request; use empty string to get the first chunk of results
  * `limit` - The maximum number of gifts to be returned; 1-100. Defaults to 100
""".
-doc (#{group=><<"Gift">>,since=><<"9.0">>}).
-spec getBusinessAccountGifts(Pool :: pool_name(), Req :: #{business_connection_id := binary(), exclude_unsaved => boolean(), exclude_saved => boolean(), exclude_unlimited => boolean(), exclude_limited_upgradable => boolean(), exclude_limited_non_upgradable => boolean(), exclude_unique => boolean(), exclude_from_blockchain => boolean(), sort_by_price => boolean(), offset => binary(), limit => integer()}, Async :: boolean()) -> Result :: result('OwnedGifts'()).
getBusinessAccountGifts(Pool, #{business_connection_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"getBusinessAccountGifts">>, Req, Async}).
-doc (#{equiv=>getBusinessAccountGifts(Pool, Req, false),since=><<"9.0">>,group=><<"Sync Request">>}).
getBusinessAccountGifts(Pool, Req) -> getBusinessAccountGifts(Pool, Req, false).


-doc """
Returns the gifts owned and hosted by a user.  
Returns OwnedGifts on success.
## Parameters
  * `user_id` - Unique identifier of the user
  * `exclude_unlimited` - Pass True to exclude gifts that can be purchased an unlimited number of times
  * `exclude_limited_upgradable` - Pass True to exclude gifts that can be purchased a limited number of times and can be upgraded to unique
  * `exclude_limited_non_upgradable` - Pass True to exclude gifts that can be purchased a limited number of times and can't be upgraded to unique
  * `exclude_from_blockchain` - Pass True to exclude gifts that were assigned from the TON blockchain and can't be resold or transferred in Telegram
  * `exclude_unique` - Pass True to exclude unique gifts
  * `sort_by_price` - Pass True to sort results by gift price instead of send date. Sorting is applied before pagination.
  * `offset` - Offset of the first entry to return as received from the previous request; use an empty string to get the first chunk of results
  * `limit` - The maximum number of gifts to be returned; 1-100. Defaults to 100
""".
-doc (#{group=><<"Gift">>,since=><<"9.3">>}).
-spec getUserGifts(Pool :: pool_name(), Req :: #{user_id := integer(), exclude_unlimited => boolean(), exclude_limited_upgradable => boolean(), exclude_limited_non_upgradable => boolean(), exclude_from_blockchain => boolean(), exclude_unique => boolean(), sort_by_price => boolean(), offset => binary(), limit => integer()}, Async :: boolean()) -> Result :: result('OwnedGifts'()).
getUserGifts(Pool, #{user_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"getUserGifts">>, Req, Async}).
-doc (#{equiv=>getUserGifts(Pool, Req, false),since=><<"9.3">>,group=><<"Sync Request">>}).
getUserGifts(Pool, Req) -> getUserGifts(Pool, Req, false).


-doc """
Returns the gifts owned by a chat.  
Returns OwnedGifts on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `exclude_unsaved` - Pass True to exclude gifts that aren't saved to the chat's profile page. Always True, unless the bot has the can_post_messages administrator right in the channel.
  * `exclude_saved` - Pass True to exclude gifts that are saved to the chat's profile page. Always False, unless the bot has the can_post_messages administrator right in the channel.
  * `exclude_unlimited` - Pass True to exclude gifts that can be purchased an unlimited number of times
  * `exclude_limited_upgradable` - Pass True to exclude gifts that can be purchased a limited number of times and can be upgraded to unique
  * `exclude_limited_non_upgradable` - Pass True to exclude gifts that can be purchased a limited number of times and can't be upgraded to unique
  * `exclude_from_blockchain` - Pass True to exclude gifts that were assigned from the TON blockchain and can't be resold or transferred in Telegram
  * `exclude_unique` - Pass True to exclude unique gifts
  * `sort_by_price` - Pass True to sort results by gift price instead of send date. Sorting is applied before pagination.
  * `offset` - Offset of the first entry to return as received from the previous request; use an empty string to get the first chunk of results
  * `limit` - The maximum number of gifts to be returned; 1-100. Defaults to 100
""".
-doc (#{group=><<"Gift">>,since=><<"9.3">>}).
-spec getChatGifts(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), exclude_unsaved => boolean(), exclude_saved => boolean(), exclude_unlimited => boolean(), exclude_limited_upgradable => boolean(), exclude_limited_non_upgradable => boolean(), exclude_from_blockchain => boolean(), exclude_unique => boolean(), sort_by_price => boolean(), offset => binary(), limit => integer()}, Async :: boolean()) -> Result :: result('OwnedGifts'()).
getChatGifts(Pool, #{chat_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"getChatGifts">>, Req, Async}).
-doc (#{equiv=>getChatGifts(Pool, Req, false),since=><<"9.3">>,group=><<"Sync Request">>}).
getChatGifts(Pool, Req) -> getChatGifts(Pool, Req, false).


-doc """
Converts a given regular gift to Telegram Stars.  
Requires the can_convert_gifts_to_stars business bot right.  
Returns True on success.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection
  * `owned_gift_id` - Unique identifier of the regular gift that should be converted to Telegram Stars
""".
-doc (#{group=><<"Payments">>,since=><<"9.0">>}).
-spec convertGiftToStars(Pool :: pool_name(), Req :: #{business_connection_id := binary(), owned_gift_id := binary()}, Async :: boolean()) -> Result :: result(true).
convertGiftToStars(Pool, #{business_connection_id:=_,owned_gift_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"convertGiftToStars">>, Req, Async}).
-doc (#{equiv=>convertGiftToStars(Pool, Req, false),since=><<"9.0">>,group=><<"Sync Request">>}).
convertGiftToStars(Pool, Req) -> convertGiftToStars(Pool, Req, false).


-doc """
Upgrades a given regular gift to a unique gift.  
Requires the can_transfer_and_upgrade_gifts business bot right.  
Additionally requires the can_transfer_stars business bot right if the upgrade is paid.  
Returns True on success.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection
  * `owned_gift_id` - Unique identifier of the regular gift that should be upgraded to a unique one
  * `keep_original_details` - Pass True to keep the original gift text, sender and receiver in the upgraded gift
  * `star_count` - The amount of Telegram Stars that will be paid for the upgrade from the business account balance. If gift.prepaid_upgrade_star_count > 0, then pass 0, otherwise, the can_transfer_stars business bot right is required and gift.upgrade_star_count must be passed.
""".
-doc (#{group=><<"Gift">>,since=><<"9.0">>}).
-spec upgradeGift(Pool :: pool_name(), Req :: #{business_connection_id := binary(), owned_gift_id := binary(), keep_original_details => boolean(), star_count => integer()}, Async :: boolean()) -> Result :: result(true).
upgradeGift(Pool, #{business_connection_id:=_,owned_gift_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"upgradeGift">>, Req, Async}).
-doc (#{equiv=>upgradeGift(Pool, Req, false),since=><<"9.0">>,group=><<"Sync Request">>}).
upgradeGift(Pool, Req) -> upgradeGift(Pool, Req, false).


-doc """
Transfers an owned unique gift to another user.  
Requires the can_transfer_and_upgrade_gifts business bot right.  
Requires can_transfer_stars business bot right if the transfer is paid.  
Returns True on success.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection
  * `owned_gift_id` - Unique identifier of the regular gift that should be transferred
  * `new_owner_chat_id` - Unique identifier of the chat which will own the gift. The chat must be active in the last 24 hours.
  * `star_count` - The amount of Telegram Stars that will be paid for the transfer from the business account balance. If positive, then the can_transfer_stars business bot right is required.
""".
-doc (#{group=><<"Gift">>,since=><<"9.0">>}).
-spec transferGift(Pool :: pool_name(), Req :: #{business_connection_id := binary(), owned_gift_id := binary(), new_owner_chat_id := integer(), star_count => integer()}, Async :: boolean()) -> Result :: result(true).
transferGift(Pool, #{business_connection_id:=_,owned_gift_id:=_,new_owner_chat_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"transferGift">>, Req, Async}).
-doc (#{equiv=>transferGift(Pool, Req, false),since=><<"9.0">>,group=><<"Sync Request">>}).
transferGift(Pool, Req) -> transferGift(Pool, Req, false).


-doc """
Posts a story on behalf of a managed business account.  
Requires the can_manage_stories business bot right.  
Returns Story on success.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection
  * `content` - Content of the story
  * `active_period` - Period after which the story is moved to the archive, in seconds; must be one of 6 * 3600, 12 * 3600, 86400, or 2 * 86400
  * `caption` - Caption of the story, 0-2048 characters after entities parsing
  * `parse_mode` - Mode for parsing entities in the story caption. See formatting options for more details.
  * `caption_entities` - A JSON-serialized list of special entities that appear in the caption, which can be specified instead of parse_mode
  * `areas` - A JSON-serialized list of clickable areas to be shown on the story
  * `post_to_chat_page` - Pass True to keep the story accessible after it expires
  * `protect_content` - Pass True if the content of the story must be protected from forwarding and screenshotting
""".
-doc (#{group=><<"Story">>,since=><<"9.0">>}).
-spec postStory(Pool :: pool_name(), Req :: #{business_connection_id := binary(), content := 'InputStoryContent'(), active_period := integer(), caption => binary(), parse_mode => binary(), caption_entities => nonempty_list('MessageEntity'()), areas => nonempty_list('StoryArea'()), post_to_chat_page => boolean(), protect_content => boolean()}, Async :: boolean()) -> Result :: result('Story'()).
postStory(Pool, #{business_connection_id:=_,content:=_,active_period:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"postStory">>, Req, Async}).
-doc (#{equiv=>postStory(Pool, Req, false),since=><<"9.0">>,group=><<"Sync Request">>}).
postStory(Pool, Req) -> postStory(Pool, Req, false).


-doc """
Reposts a story on behalf of a business account from another business account.  
Both business accounts must be managed by the same bot, and the story on the source account must have been posted (or reposted) by the bot.  
Requires the can_manage_stories business bot right for both business accounts.  
Returns Story on success.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection
  * `from_chat_id` - Unique identifier of the chat which posted the story that should be reposted
  * `from_story_id` - Unique identifier of the story that should be reposted
  * `active_period` - Period after which the story is moved to the archive, in seconds; must be one of 6 * 3600, 12 * 3600, 86400, or 2 * 86400
  * `post_to_chat_page` - Pass True to keep the story accessible after it expires
  * `protect_content` - Pass True if the content of the story must be protected from forwarding and screenshotting
""".
-doc (#{group=><<"Story">>,since=><<"9.3">>}).
-spec repostStory(Pool :: pool_name(), Req :: #{business_connection_id := binary(), from_chat_id := integer(), from_story_id := integer(), active_period := integer(), post_to_chat_page => boolean(), protect_content => boolean()}, Async :: boolean()) -> Result :: result('Story'()).
repostStory(Pool, #{business_connection_id:=_,from_chat_id:=_,from_story_id:=_,active_period:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"repostStory">>, Req, Async}).
-doc (#{equiv=>repostStory(Pool, Req, false),since=><<"9.3">>,group=><<"Sync Request">>}).
repostStory(Pool, Req) -> repostStory(Pool, Req, false).


-doc """
Edits a story previously posted by the bot on behalf of a managed business account.  
Requires the can_manage_stories business bot right.  
Returns Story on success.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection
  * `story_id` - Unique identifier of the story to edit
  * `content` - Content of the story
  * `caption` - Caption of the story, 0-2048 characters after entities parsing
  * `parse_mode` - Mode for parsing entities in the story caption. See formatting options for more details.
  * `caption_entities` - A JSON-serialized list of special entities that appear in the caption, which can be specified instead of parse_mode
  * `areas` - A JSON-serialized list of clickable areas to be shown on the story
""".
-doc (#{group=><<"Story">>,since=><<"9.0">>}).
-spec editStory(Pool :: pool_name(), Req :: #{business_connection_id := binary(), story_id := integer(), content := 'InputStoryContent'(), caption => binary(), parse_mode => binary(), caption_entities => nonempty_list('MessageEntity'()), areas => nonempty_list('StoryArea'())}, Async :: boolean()) -> Result :: result('Story'()).
editStory(Pool, #{business_connection_id:=_,story_id:=_,content:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"editStory">>, Req, Async}).
-doc (#{equiv=>editStory(Pool, Req, false),since=><<"9.0">>,group=><<"Sync Request">>}).
editStory(Pool, Req) -> editStory(Pool, Req, false).


-doc """
Deletes a story previously posted by the bot on behalf of a managed business account.  
Requires the can_manage_stories business bot right.  
Returns True on success.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection
  * `story_id` - Unique identifier of the story to delete
""".
-doc (#{group=><<"Story">>,since=><<"9.0">>}).
-spec deleteStory(Pool :: pool_name(), Req :: #{business_connection_id := binary(), story_id := integer()}, Async :: boolean()) -> Result :: result(true).
deleteStory(Pool, #{business_connection_id:=_,story_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"deleteStory">>, Req, Async}).
-doc (#{equiv=>deleteStory(Pool, Req, false),since=><<"9.0">>,group=><<"Sync Request">>}).
deleteStory(Pool, Req) -> deleteStory(Pool, Req, false).


-doc """
Use this method to edit text and game messages.  
On success, if the edited message is not an inline message, the edited Message is returned, otherwise True is returned.  
Note that business messages that were not sent by the bot and do not contain an inline keyboard can only be edited within 48 hours from the time they were sent.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the message to be edited was sent
  * `chat_id` - Required if inline_message_id is not specified. Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_id` - Required if inline_message_id is not specified. Identifier of the message to edit
  * `inline_message_id` - Required if chat_id and message_id are not specified. Identifier of the inline message
  * `text` - New text of the message, 1-4096 characters after entities parsing
  * `parse_mode` - Mode for parsing entities in the message text. See formatting options for more details.
  * `entities` - A JSON-serialized list of special entities that appear in message text, which can be specified instead of parse_mode
  * `link_preview_options` - Link preview generation options for the message
  * `reply_markup` - A JSON-serialized object for an inline keyboard.
""".
-doc (#{group=><<"Message">>,since=><<"2.0">>}).
-spec editMessageText(Pool :: pool_name(), Req :: #{business_connection_id => binary(), chat_id => integer() | binary(), message_id => integer(), inline_message_id => binary(), text := binary(), parse_mode => binary(), entities => nonempty_list('MessageEntity'()), link_preview_options => 'LinkPreviewOptions'(), reply_markup => 'InlineKeyboardMarkup'()}, Async :: boolean()) -> Result :: result(true | 'Message'()).
editMessageText(Pool, #{text:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"editMessageText">>, Req, Async}).
-doc (#{equiv=>editMessageText(Pool, Req, false),since=><<"2.0">>,group=><<"Sync Request">>}).
editMessageText(Pool, Req) -> editMessageText(Pool, Req, false).


-doc """
Use this method to edit captions of messages.  
On success, if the edited message is not an inline message, the edited Message is returned, otherwise True is returned.  
Note that business messages that were not sent by the bot and do not contain an inline keyboard can only be edited within 48 hours from the time they were sent.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the message to be edited was sent
  * `chat_id` - Required if inline_message_id is not specified. Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_id` - Required if inline_message_id is not specified. Identifier of the message to edit
  * `inline_message_id` - Required if chat_id and message_id are not specified. Identifier of the inline message
  * `caption` - New caption of the message, 0-1024 characters after entities parsing
  * `parse_mode` - Mode for parsing entities in the message caption. See formatting options for more details.
  * `caption_entities` - A JSON-serialized list of special entities that appear in the caption, which can be specified instead of parse_mode
  * `show_caption_above_media` - Pass True, if the caption must be shown above the message media. Supported only for animation, photo and video messages.
  * `reply_markup` - A JSON-serialized object for an inline keyboard.
""".
-doc (#{group=><<"Message">>,since=><<"2.0">>}).
-spec editMessageCaption(Pool :: pool_name(), Req :: #{business_connection_id => binary(), chat_id => integer() | binary(), message_id => integer(), inline_message_id => binary(), caption => binary(), parse_mode => binary(), caption_entities => nonempty_list('MessageEntity'()), show_caption_above_media => boolean(), reply_markup => 'InlineKeyboardMarkup'()}, Async :: boolean()) -> Result :: result(true | 'Message'()).
editMessageCaption(Pool, #{} = Req, Async) ->wpool:call(Pool, {raw, <<"editMessageCaption">>, Req, Async}).
-doc (#{equiv=>editMessageCaption(Pool, Req, false),since=><<"2.0">>,group=><<"Sync Request">>}).
editMessageCaption(Pool, Req) -> editMessageCaption(Pool, Req, false).


-doc """
Use this method to edit animation, audio, document, photo, or video messages, or to add media to text messages.  
If a message is part of a message album, then it can be edited only to an audio for audio albums, only to a document for document albums and to a photo or a video otherwise.  
When an inline message is edited, a new file can't be uploaded; use a previously uploaded file via its file_id or specify a URL.  
On success, if the edited message is not an inline message, the edited Message is returned, otherwise True is returned.  
Note that business messages that were not sent by the bot and do not contain an inline keyboard can only be edited within 48 hours from the time they were sent.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the message to be edited was sent
  * `chat_id` - Required if inline_message_id is not specified. Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_id` - Required if inline_message_id is not specified. Identifier of the message to edit
  * `inline_message_id` - Required if chat_id and message_id are not specified. Identifier of the inline message
  * `media` - A JSON-serialized object for a new media content of the message
  * `reply_markup` - A JSON-serialized object for a new inline keyboard.
""".
-doc (#{group=><<"Message">>,since=><<"4.0">>}).
-spec editMessageMedia(Pool :: pool_name(), Req :: #{business_connection_id => binary(), chat_id => integer() | binary(), message_id => integer(), inline_message_id => binary(), media := 'InputMedia'(), reply_markup => 'InlineKeyboardMarkup'()}, Async :: boolean()) -> Result :: result(true | 'Message'()).
editMessageMedia(Pool, #{media:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"editMessageMedia">>, Req, Async}).
-doc (#{equiv=>editMessageMedia(Pool, Req, false),since=><<"4.0">>,group=><<"Sync Request">>}).
editMessageMedia(Pool, Req) -> editMessageMedia(Pool, Req, false).


-doc """
Use this method to edit live location messages.  
A location can be edited until its live_period expires or editing is explicitly disabled by a call to stopMessageLiveLocation.  
On success, if the edited message is not an inline message, the edited Message is returned, otherwise True is returned.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the message to be edited was sent
  * `chat_id` - Required if inline_message_id is not specified. Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_id` - Required if inline_message_id is not specified. Identifier of the message to edit
  * `inline_message_id` - Required if chat_id and message_id are not specified. Identifier of the inline message
  * `latitude` - Latitude of new location
  * `longitude` - Longitude of new location
  * `live_period` - New period in seconds during which the location can be updated, starting from the message send date. If 0x7FFFFFFF is specified, then the location can be updated forever. Otherwise, the new value must not exceed the current live_period by more than a day, and the live location expiration date must remain within the next 90 days. If not specified, then live_period remains unchanged
  * `horizontal_accuracy` - The radius of uncertainty for the location, measured in meters; 0-1500
  * `heading` - Direction in which the user is moving, in degrees. Must be between 1 and 360 if specified.
  * `proximity_alert_radius` - The maximum distance for proximity alerts about approaching another chat member, in meters. Must be between 1 and 100000 if specified.
  * `reply_markup` - A JSON-serialized object for a new inline keyboard.
""".
-doc (#{group=><<"Message">>,since=><<"3.4">>}).
-spec editMessageLiveLocation(Pool :: pool_name(), Req :: #{business_connection_id => binary(), chat_id => integer() | binary(), message_id => integer(), inline_message_id => binary(), latitude := float(), longitude := float(), live_period => integer(), horizontal_accuracy => float(), heading => integer(), proximity_alert_radius => integer(), reply_markup => 'InlineKeyboardMarkup'()}, Async :: boolean()) -> Result :: result(true | 'Message'()).
editMessageLiveLocation(Pool, #{latitude:=_,longitude:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"editMessageLiveLocation">>, Req, Async}).
-doc (#{equiv=>editMessageLiveLocation(Pool, Req, false),since=><<"3.4">>,group=><<"Sync Request">>}).
editMessageLiveLocation(Pool, Req) -> editMessageLiveLocation(Pool, Req, false).


-doc """
Use this method to stop updating a live location message before live_period expires.  
On success, if the message is not an inline message, the edited Message is returned, otherwise True is returned.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the message to be edited was sent
  * `chat_id` - Required if inline_message_id is not specified. Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_id` - Required if inline_message_id is not specified. Identifier of the message with live location to stop
  * `inline_message_id` - Required if chat_id and message_id are not specified. Identifier of the inline message
  * `reply_markup` - A JSON-serialized object for a new inline keyboard.
""".
-doc (#{group=><<"Message">>,since=><<"3.4">>}).
-spec stopMessageLiveLocation(Pool :: pool_name(), Req :: #{business_connection_id => binary(), chat_id => integer() | binary(), message_id => integer(), inline_message_id => binary(), reply_markup => 'InlineKeyboardMarkup'()}, Async :: boolean()) -> Result :: result(true | 'Message'()).
stopMessageLiveLocation(Pool, #{} = Req, Async) ->wpool:call(Pool, {raw, <<"stopMessageLiveLocation">>, Req, Async}).
-doc (#{equiv=>stopMessageLiveLocation(Pool, Req, false),since=><<"3.4">>,group=><<"Sync Request">>}).
stopMessageLiveLocation(Pool, Req) -> stopMessageLiveLocation(Pool, Req, false).


-doc """
Use this method to edit a checklist on behalf of a connected business account.  
On success, the edited Message is returned.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the message will be sent
  * `chat_id` - Unique identifier for the target chat
  * `message_id` - Unique identifier for the target message
  * `checklist` - A JSON-serialized object for the new checklist
  * `reply_markup` - A JSON-serialized object for the new inline keyboard for the message
""".
-doc (#{group=><<"Message">>,since=><<"9.1">>}).
-spec editMessageChecklist(Pool :: pool_name(), Req :: #{business_connection_id := binary(), chat_id := integer(), message_id := integer(), checklist := 'InputChecklist'(), reply_markup => 'InlineKeyboardMarkup'()}, Async :: boolean()) -> Result :: result('Message'()).
editMessageChecklist(Pool, #{business_connection_id:=_,chat_id:=_,message_id:=_,checklist:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"editMessageChecklist">>, Req, Async}).
-doc (#{equiv=>editMessageChecklist(Pool, Req, false),since=><<"9.1">>,group=><<"Sync Request">>}).
editMessageChecklist(Pool, Req) -> editMessageChecklist(Pool, Req, false).


-doc """
Use this method to edit only the reply markup of messages.  
On success, if the edited message is not an inline message, the edited Message is returned, otherwise True is returned.  
Note that business messages that were not sent by the bot and do not contain an inline keyboard can only be edited within 48 hours from the time they were sent.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the message to be edited was sent
  * `chat_id` - Required if inline_message_id is not specified. Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_id` - Required if inline_message_id is not specified. Identifier of the message to edit
  * `inline_message_id` - Required if chat_id and message_id are not specified. Identifier of the inline message
  * `reply_markup` - A JSON-serialized object for an inline keyboard.
""".
-doc (#{group=><<"Message">>,since=><<"2.0">>}).
-spec editMessageReplyMarkup(Pool :: pool_name(), Req :: #{business_connection_id => binary(), chat_id => integer() | binary(), message_id => integer(), inline_message_id => binary(), reply_markup => 'InlineKeyboardMarkup'()}, Async :: boolean()) -> Result :: result(true | 'Message'()).
editMessageReplyMarkup(Pool, #{} = Req, Async) ->wpool:call(Pool, {raw, <<"editMessageReplyMarkup">>, Req, Async}).
-doc (#{equiv=>editMessageReplyMarkup(Pool, Req, false),since=><<"2.0">>,group=><<"Sync Request">>}).
editMessageReplyMarkup(Pool, Req) -> editMessageReplyMarkup(Pool, Req, false).


-doc """
Use this method to stop a poll which was sent by the bot.  
On success, the stopped Poll is returned.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the message to be edited was sent
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_id` - Identifier of the original message with the poll
  * `reply_markup` - A JSON-serialized object for a new message inline keyboard.
""".
-doc (#{group=><<"Message">>,since=><<"4.2">>}).
-spec stopPoll(Pool :: pool_name(), Req :: #{business_connection_id => binary(), chat_id := integer() | binary(), message_id := integer(), reply_markup => 'InlineKeyboardMarkup'()}, Async :: boolean()) -> Result :: result('Poll'()).
stopPoll(Pool, #{chat_id:=_,message_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"stopPoll">>, Req, Async}).
-doc (#{equiv=>stopPoll(Pool, Req, false),since=><<"4.2">>,group=><<"Sync Request">>}).
stopPoll(Pool, Req) -> stopPoll(Pool, Req, false).


-doc """
Use this method to approve a suggested post in a direct messages chat.  
The bot must have the 'can_post_messages' administrator right in the corresponding channel chat.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target direct messages chat
  * `message_id` - Identifier of a suggested post message to approve
  * `send_date` - Point in time (Unix timestamp) when the post is expected to be published; omit if the date has already been specified when the suggested post was created. If specified, then the date must be not more than 2678400 seconds (30 days) in the future
""".
-doc (#{group=><<"Message">>,since=><<"9.2">>}).
-spec approveSuggestedPost(Pool :: pool_name(), Req :: #{chat_id := integer(), message_id := integer(), send_date => integer()}, Async :: boolean()) -> Result :: result(true).
approveSuggestedPost(Pool, #{chat_id:=_,message_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"approveSuggestedPost">>, Req, Async}).
-doc (#{equiv=>approveSuggestedPost(Pool, Req, false),since=><<"9.2">>,group=><<"Sync Request">>}).
approveSuggestedPost(Pool, Req) -> approveSuggestedPost(Pool, Req, false).


-doc """
Use this method to decline a suggested post in a direct messages chat.  
The bot must have the 'can_manage_direct_messages' administrator right in the corresponding channel chat.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target direct messages chat
  * `message_id` - Identifier of a suggested post message to decline
  * `comment` - Comment for the creator of the suggested post; 0-128 characters
""".
-doc (#{group=><<"Message">>,since=><<"9.2">>}).
-spec declineSuggestedPost(Pool :: pool_name(), Req :: #{chat_id := integer(), message_id := integer(), comment => binary()}, Async :: boolean()) -> Result :: result(true).
declineSuggestedPost(Pool, #{chat_id:=_,message_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"declineSuggestedPost">>, Req, Async}).
-doc (#{equiv=>declineSuggestedPost(Pool, Req, false),since=><<"9.2">>,group=><<"Sync Request">>}).
declineSuggestedPost(Pool, Req) -> declineSuggestedPost(Pool, Req, false).


-doc """
Use this method to delete a message, including service messages, with the following limitations:
- A message can only be deleted if it was sent less than 48 hours ago.  
- Service messages about a supergroup, channel, or forum topic creation can't be deleted.  
- A dice message in a private chat can only be deleted if it was sent more than 24 hours ago.  
- Bots can delete outgoing messages in private chats, groups, and supergroups.  
- Bots can delete incoming messages in private chats.  
- Bots granted can_post_messages permissions can delete outgoing messages in channels.  
- If the bot is an administrator of a group, it can delete any message there.  
- If the bot has can_delete_messages administrator right in a supergroup or a channel, it can delete any message there.  
- If the bot has can_manage_direct_messages administrator right in a channel, it can delete any message in the corresponding direct messages chat.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_id` - Identifier of the message to delete
""".
-doc (#{group=><<"Message">>,since=><<"3.0">>}).
-spec deleteMessage(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), message_id := integer()}, Async :: boolean()) -> Result :: result(true).
deleteMessage(Pool, #{chat_id:=_,message_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"deleteMessage">>, Req, Async}).
-doc (#{equiv=>deleteMessage(Pool, Req, false),since=><<"3.0">>,group=><<"Sync Request">>}).
deleteMessage(Pool, Req) -> deleteMessage(Pool, Req, false).


-doc """
Use this method to delete multiple messages simultaneously.  
If some of the specified messages can't be found, they are skipped.  
Returns True on success.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_ids` - A JSON-serialized list of 1-100 identifiers of messages to delete. See deleteMessage for limitations on which messages can be deleted
""".
-doc (#{group=><<"Message">>,since=><<"7.0">>}).
-spec deleteMessages(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), message_ids := nonempty_list(integer())}, Async :: boolean()) -> Result :: result(true).
deleteMessages(Pool, #{chat_id:=_,message_ids:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"deleteMessages">>, Req, Async}).
-doc (#{equiv=>deleteMessages(Pool, Req, false),since=><<"7.0">>,group=><<"Sync Request">>}).
deleteMessages(Pool, Req) -> deleteMessages(Pool, Req, false).


-doc """
Use this method to send static .WEBP, animated .TGS, or video .WEBM stickers.  
On success, the sent Message is returned.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the message will be sent
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_thread_id` - Unique identifier for the target message thread (topic) of a forum; for forum supergroups and private chats of bots with forum topic mode enabled only
  * `direct_messages_topic_id` - Identifier of the direct messages topic to which the message will be sent; required if the message is sent to a direct messages chat
  * `sticker` - Sticker to send. Pass a file_id as String to send a file that exists on the Telegram servers (recommended), pass an HTTP URL as a String for Telegram to get a .WEBP sticker from the Internet, or upload a new .WEBP, .TGS, or .WEBM sticker using multipart/form-data. More information on Sending Files ». Video and animated stickers can't be sent via an HTTP URL.
  * `emoji` - Emoji associated with the sticker; only for just uploaded stickers
  * `disable_notification` - Sends the message silently. Users will receive a notification with no sound.
  * `protect_content` - Protects the contents of the sent message from forwarding and saving
  * `allow_paid_broadcast` - Pass True to allow up to 1000 messages per second, ignoring broadcasting limits for a fee of 0.1 Telegram Stars per message. The relevant Stars will be withdrawn from the bot's balance
  * `message_effect_id` - Unique identifier of the message effect to be added to the message; for private chats only
  * `suggested_post_parameters` - A JSON-serialized object containing the parameters of the suggested post to send; for direct messages chats only. If the message is sent as a reply to another suggested post, then that suggested post is automatically declined.
  * `reply_parameters` - Description of the message to reply to
  * `reply_markup` - Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove a reply keyboard or to force a reply from the user
""".
-doc (#{group=><<"Sticker">>,since=><<"4.4">>}).
-spec sendSticker(Pool :: pool_name(), Req :: #{business_connection_id => binary(), chat_id := integer() | binary(), message_thread_id => integer(), direct_messages_topic_id => integer(), sticker := 'InputFile'() | binary(), emoji => binary(), disable_notification => boolean(), protect_content => boolean(), allow_paid_broadcast => boolean(), message_effect_id => binary(), suggested_post_parameters => 'SuggestedPostParameters'(), reply_parameters => 'ReplyParameters'(), reply_markup => 'InlineKeyboardMarkup'() | 'ReplyKeyboardMarkup'() | 'ReplyKeyboardRemove'() | 'ForceReply'()}, Async :: boolean()) -> Result :: result('Message'()).
sendSticker(Pool, #{chat_id:=_,sticker:=_} = Req, Async) ->wpool:call(Pool, {multipart, <<"sendSticker">>, Req, Async}).
-doc (#{equiv=>sendSticker(Pool, Req, false),since=><<"4.4">>,group=><<"Sync Request">>}).
sendSticker(Pool, Req) -> sendSticker(Pool, Req, false).


-doc """
Use this method to get a sticker set.  
On success, a StickerSet object is returned.
## Parameters
  * `name` - Name of the sticker set
""".
-doc (#{group=><<"Sticker">>,since=><<"3.2">>}).
-spec getStickerSet(Pool :: pool_name(), Req :: #{name := binary()}, Async :: boolean()) -> Result :: result('StickerSet'()).
getStickerSet(Pool, #{name:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"getStickerSet">>, Req, Async}).
-doc (#{equiv=>getStickerSet(Pool, Req, false),since=><<"3.2">>,group=><<"Sync Request">>}).
getStickerSet(Pool, Req) -> getStickerSet(Pool, Req, false).


-doc """
Use this method to get information about custom emoji stickers by their identifiers.  
Returns an Array of Sticker objects.
## Parameters
  * `custom_emoji_ids` - A JSON-serialized list of custom emoji identifiers. At most 200 custom emoji identifiers can be specified.
""".
-doc (#{group=><<"Sticker">>,since=><<"6.2">>}).
-spec getCustomEmojiStickers(Pool :: pool_name(), Req :: #{custom_emoji_ids := nonempty_list(binary())}, Async :: boolean()) -> Result :: result(nonempty_list('Sticker'())).
getCustomEmojiStickers(Pool, #{custom_emoji_ids:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"getCustomEmojiStickers">>, Req, Async}).
-doc (#{equiv=>getCustomEmojiStickers(Pool, Req, false),since=><<"6.2">>,group=><<"Sync Request">>}).
getCustomEmojiStickers(Pool, Req) -> getCustomEmojiStickers(Pool, Req, false).


-doc """
Use this method to upload a file with a sticker for later use in the createNewStickerSet, addStickerToSet, or replaceStickerInSet methods (the file can be used multiple times).  
Returns the uploaded File on success.
## Parameters
  * `user_id` - User identifier of sticker file owner
  * `sticker` - A file with the sticker in .WEBP, .PNG, .TGS, or .WEBM format. See https://core.telegram.org/stickers for technical requirements. More information on Sending Files »
  * `sticker_format` - Format of the sticker, must be one of “static”, “animated”, “video”
""".
-doc (#{group=><<"Sticker">>,since=><<"3.2">>}).
-spec uploadStickerFile(Pool :: pool_name(), Req :: #{user_id := integer(), sticker := 'InputFile'(), sticker_format := binary()}, Async :: boolean()) -> Result :: result('File'()).
uploadStickerFile(Pool, #{user_id:=_,sticker:=_,sticker_format:=_} = Req, Async) ->wpool:call(Pool, {multipart, <<"uploadStickerFile">>, Req, Async}).
-doc (#{equiv=>uploadStickerFile(Pool, Req, false),since=><<"3.2">>,group=><<"Sync Request">>}).
uploadStickerFile(Pool, Req) -> uploadStickerFile(Pool, Req, false).


-doc """
Use this method to create a new sticker set owned by a user.  
The bot will be able to edit the sticker set thus created.  
Returns True on success.
## Parameters
  * `user_id` - User identifier of created sticker set owner
  * `name` - Short name of sticker set, to be used in t.me/addstickers/ URLs (e.g., animals). Can contain only English letters, digits and underscores. Must begin with a letter, can't contain consecutive underscores and must end in _by_<bot_username>. <bot_username> is case insensitive. 1-64 characters.
  * `title` - Sticker set title, 1-64 characters
  * `stickers` - A JSON-serialized list of 1-50 initial stickers to be added to the sticker set
  * `sticker_type` - Type of stickers in the set, pass “regular”, “mask”, or “custom_emoji”. By default, a regular sticker set is created.
  * `needs_repainting` - Pass True if stickers in the sticker set must be repainted to the color of text when used in messages, the accent color if used as emoji status, white on chat photos, or another appropriate color based on context; for custom emoji sticker sets only
""".
-doc (#{group=><<"Sticker">>,since=><<"3.2">>}).
-spec createNewStickerSet(Pool :: pool_name(), Req :: #{user_id := integer(), name := binary(), title := binary(), stickers := nonempty_list('InputSticker'()), sticker_type => binary(), needs_repainting => boolean()}, Async :: boolean()) -> Result :: result(true).
createNewStickerSet(Pool, #{user_id:=_,name:=_,title:=_,stickers:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"createNewStickerSet">>, Req, Async}).
-doc (#{equiv=>createNewStickerSet(Pool, Req, false),since=><<"3.2">>,group=><<"Sync Request">>}).
createNewStickerSet(Pool, Req) -> createNewStickerSet(Pool, Req, false).


-doc """
Use this method to add a new sticker to a set created by the bot.  
Emoji sticker sets can have up to 200 stickers.  
Other sticker sets can have up to 120 stickers.  
Returns True on success.
## Parameters
  * `user_id` - User identifier of sticker set owner
  * `name` - Sticker set name
  * `sticker` - A JSON-serialized object with information about the added sticker. If exactly the same sticker had already been added to the set, then the set isn't changed.
""".
-doc (#{group=><<"Sticker">>,since=><<"3.2">>}).
-spec addStickerToSet(Pool :: pool_name(), Req :: #{user_id := integer(), name := binary(), sticker := 'InputSticker'()}, Async :: boolean()) -> Result :: result(true).
addStickerToSet(Pool, #{user_id:=_,name:=_,sticker:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"addStickerToSet">>, Req, Async}).
-doc (#{equiv=>addStickerToSet(Pool, Req, false),since=><<"3.2">>,group=><<"Sync Request">>}).
addStickerToSet(Pool, Req) -> addStickerToSet(Pool, Req, false).


-doc """
Use this method to move a sticker in a set created by the bot to a specific position.  
Returns True on success.
## Parameters
  * `sticker` - File identifier of the sticker
  * `position` - New sticker position in the set, zero-based
""".
-doc (#{group=><<"Sticker">>,since=><<"3.2">>}).
-spec setStickerPositionInSet(Pool :: pool_name(), Req :: #{sticker := binary(), position := integer()}, Async :: boolean()) -> Result :: result(true).
setStickerPositionInSet(Pool, #{sticker:=_,position:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"setStickerPositionInSet">>, Req, Async}).
-doc (#{equiv=>setStickerPositionInSet(Pool, Req, false),since=><<"3.2">>,group=><<"Sync Request">>}).
setStickerPositionInSet(Pool, Req) -> setStickerPositionInSet(Pool, Req, false).


-doc """
Use this method to delete a sticker from a set created by the bot.  
Returns True on success.
## Parameters
  * `sticker` - File identifier of the sticker
""".
-doc (#{group=><<"Sticker">>,since=><<"3.2">>}).
-spec deleteStickerFromSet(Pool :: pool_name(), Req :: #{sticker := binary()}, Async :: boolean()) -> Result :: result(true).
deleteStickerFromSet(Pool, #{sticker:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"deleteStickerFromSet">>, Req, Async}).
-doc (#{equiv=>deleteStickerFromSet(Pool, Req, false),since=><<"3.2">>,group=><<"Sync Request">>}).
deleteStickerFromSet(Pool, Req) -> deleteStickerFromSet(Pool, Req, false).


-doc """
Use this method to replace an existing sticker in a sticker set with a new one.  
The method is equivalent to calling deleteStickerFromSet, then addStickerToSet, then setStickerPositionInSet.  
Returns True on success.
## Parameters
  * `user_id` - User identifier of the sticker set owner
  * `name` - Sticker set name
  * `old_sticker` - File identifier of the replaced sticker
  * `sticker` - A JSON-serialized object with information about the added sticker. If exactly the same sticker had already been added to the set, then the set remains unchanged.
""".
-doc (#{group=><<"Sticker">>,since=><<"7.2">>}).
-spec replaceStickerInSet(Pool :: pool_name(), Req :: #{user_id := integer(), name := binary(), old_sticker := binary(), sticker := 'InputSticker'()}, Async :: boolean()) -> Result :: result(true).
replaceStickerInSet(Pool, #{user_id:=_,name:=_,old_sticker:=_,sticker:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"replaceStickerInSet">>, Req, Async}).
-doc (#{equiv=>replaceStickerInSet(Pool, Req, false),since=><<"7.2">>,group=><<"Sync Request">>}).
replaceStickerInSet(Pool, Req) -> replaceStickerInSet(Pool, Req, false).


-doc """
Use this method to change the list of emoji assigned to a regular or custom emoji sticker.  
The sticker must belong to a sticker set created by the bot.  
Returns True on success.
## Parameters
  * `sticker` - File identifier of the sticker
  * `emoji_list` - A JSON-serialized list of 1-20 emoji associated with the sticker
""".
-doc (#{group=><<"Sticker">>,since=><<"6.6">>}).
-spec setStickerEmojiList(Pool :: pool_name(), Req :: #{sticker := binary(), emoji_list := nonempty_list(binary())}, Async :: boolean()) -> Result :: result(true).
setStickerEmojiList(Pool, #{sticker:=_,emoji_list:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"setStickerEmojiList">>, Req, Async}).
-doc (#{equiv=>setStickerEmojiList(Pool, Req, false),since=><<"6.6">>,group=><<"Sync Request">>}).
setStickerEmojiList(Pool, Req) -> setStickerEmojiList(Pool, Req, false).


-doc """
Use this method to change search keywords assigned to a regular or custom emoji sticker.  
The sticker must belong to a sticker set created by the bot.  
Returns True on success.
## Parameters
  * `sticker` - File identifier of the sticker
  * `keywords` - A JSON-serialized list of 0-20 search keywords for the sticker with total length of up to 64 characters
""".
-doc (#{group=><<"Sticker">>,since=><<"6.6">>}).
-spec setStickerKeywords(Pool :: pool_name(), Req :: #{sticker := binary(), keywords => nonempty_list(binary())}, Async :: boolean()) -> Result :: result(true).
setStickerKeywords(Pool, #{sticker:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"setStickerKeywords">>, Req, Async}).
-doc (#{equiv=>setStickerKeywords(Pool, Req, false),since=><<"6.6">>,group=><<"Sync Request">>}).
setStickerKeywords(Pool, Req) -> setStickerKeywords(Pool, Req, false).


-doc """
Use this method to change the mask position of a mask sticker.  
The sticker must belong to a sticker set that was created by the bot.  
Returns True on success.
## Parameters
  * `sticker` - File identifier of the sticker
  * `mask_position` - A JSON-serialized object with the position where the mask should be placed on faces. Omit the parameter to remove the mask position.
""".
-doc (#{group=><<"Sticker">>,since=><<"6.6">>}).
-spec setStickerMaskPosition(Pool :: pool_name(), Req :: #{sticker := binary(), mask_position => 'MaskPosition'()}, Async :: boolean()) -> Result :: result(true).
setStickerMaskPosition(Pool, #{sticker:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"setStickerMaskPosition">>, Req, Async}).
-doc (#{equiv=>setStickerMaskPosition(Pool, Req, false),since=><<"6.6">>,group=><<"Sync Request">>}).
setStickerMaskPosition(Pool, Req) -> setStickerMaskPosition(Pool, Req, false).


-doc """
Use this method to set the title of a created sticker set.  
Returns True on success.
## Parameters
  * `name` - Sticker set name
  * `title` - Sticker set title, 1-64 characters
""".
-doc (#{group=><<"Sticker">>,since=><<"6.6">>}).
-spec setStickerSetTitle(Pool :: pool_name(), Req :: #{name := binary(), title := binary()}, Async :: boolean()) -> Result :: result(true).
setStickerSetTitle(Pool, #{name:=_,title:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"setStickerSetTitle">>, Req, Async}).
-doc (#{equiv=>setStickerSetTitle(Pool, Req, false),since=><<"6.6">>,group=><<"Sync Request">>}).
setStickerSetTitle(Pool, Req) -> setStickerSetTitle(Pool, Req, false).


-doc """
Use this method to set the thumbnail of a regular or mask sticker set.  
The format of the thumbnail file must match the format of the stickers in the set.  
Returns True on success.
## Parameters
  * `name` - Sticker set name
  * `user_id` - User identifier of the sticker set owner
  * `thumbnail` - A .WEBP or .PNG image with the thumbnail, must be up to 128 kilobytes in size and have a width and height of exactly 100px, or a .TGS animation with a thumbnail up to 32 kilobytes in size (see https://core.telegram.org/stickers#animation-requirements for animated sticker technical requirements), or a .WEBM video with the thumbnail up to 32 kilobytes in size; see https://core.telegram.org/stickers#video-requirements for video sticker technical requirements. Pass a file_id as a String to send a file that already exists on the Telegram servers, pass an HTTP URL as a String for Telegram to get a file from the Internet, or upload a new one using multipart/form-data. More information on Sending Files ». Animated and video sticker set thumbnails can't be uploaded via HTTP URL. If omitted, then the thumbnail is dropped and the first sticker is used as the thumbnail.
  * `format` - Format of the thumbnail, must be one of “static” for a .WEBP or .PNG image, “animated” for a .TGS animation, or “video” for a .WEBM video
""".
-doc (#{group=><<"Sticker">>,since=><<"6.6">>}).
-spec setStickerSetThumbnail(Pool :: pool_name(), Req :: #{name := binary(), user_id := integer(), thumbnail => 'InputFile'() | binary(), format := binary()}, Async :: boolean()) -> Result :: result(true).
setStickerSetThumbnail(Pool, #{name:=_,user_id:=_,format:=_} = Req, Async) ->wpool:call(Pool, {multipart, <<"setStickerSetThumbnail">>, Req, Async}).
-doc (#{equiv=>setStickerSetThumbnail(Pool, Req, false),since=><<"6.6">>,group=><<"Sync Request">>}).
setStickerSetThumbnail(Pool, Req) -> setStickerSetThumbnail(Pool, Req, false).


-doc """
Use this method to set the thumbnail of a custom emoji sticker set.  
Returns True on success.
## Parameters
  * `name` - Sticker set name
  * `custom_emoji_id` - Custom emoji identifier of a sticker from the sticker set; pass an empty string to drop the thumbnail and use the first sticker as the thumbnail.
""".
-doc (#{group=><<"Sticker">>,since=><<"6.6">>}).
-spec setCustomEmojiStickerSetThumbnail(Pool :: pool_name(), Req :: #{name := binary(), custom_emoji_id => binary()}, Async :: boolean()) -> Result :: result(true).
setCustomEmojiStickerSetThumbnail(Pool, #{name:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"setCustomEmojiStickerSetThumbnail">>, Req, Async}).
-doc (#{equiv=>setCustomEmojiStickerSetThumbnail(Pool, Req, false),since=><<"6.6">>,group=><<"Sync Request">>}).
setCustomEmojiStickerSetThumbnail(Pool, Req) -> setCustomEmojiStickerSetThumbnail(Pool, Req, false).


-doc """
Use this method to delete a sticker set that was created by the bot.  
Returns True on success.
## Parameters
  * `name` - Sticker set name
""".
-doc (#{group=><<"Sticker">>,since=><<"6.6">>}).
-spec deleteStickerSet(Pool :: pool_name(), Req :: #{name := binary()}, Async :: boolean()) -> Result :: result(true).
deleteStickerSet(Pool, #{name:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"deleteStickerSet">>, Req, Async}).
-doc (#{equiv=>deleteStickerSet(Pool, Req, false),since=><<"6.6">>,group=><<"Sync Request">>}).
deleteStickerSet(Pool, Req) -> deleteStickerSet(Pool, Req, false).


-doc """
Use this method to send answers to an inline query.  
On success, True is returned.  
No more than 50 results per query are allowed.
## Parameters
  * `inline_query_id` - Unique identifier for the answered query
  * `results` - A JSON-serialized array of results for the inline query
  * `cache_time` - The maximum amount of time in seconds that the result of the inline query may be cached on the server. Defaults to 300.
  * `is_personal` - Pass True if results may be cached on the server side only for the user that sent the query. By default, results may be returned to any user who sends the same query.
  * `next_offset` - Pass the offset that a client should send in the next query with the same text to receive more results. Pass an empty string if there are no more results or if you don't support pagination. Offset length can't exceed 64 bytes.
  * `button` - A JSON-serialized object describing a button to be shown above inline query results
""".
-doc (#{group=><<"Inline Mode">>,since=><<"1.16">>}).
-spec answerInlineQuery(Pool :: pool_name(), Req :: #{inline_query_id := binary(), results := nonempty_list('InlineQueryResult'()), cache_time => integer(), is_personal => boolean(), next_offset => binary(), button => 'InlineQueryResultsButton'()}, Async :: boolean()) -> Result :: result(true).
answerInlineQuery(Pool, #{inline_query_id:=_,results:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"answerInlineQuery">>, Req, Async}).
-doc (#{equiv=>answerInlineQuery(Pool, Req, false),since=><<"1.16">>,group=><<"Sync Request">>}).
answerInlineQuery(Pool, Req) -> answerInlineQuery(Pool, Req, false).


-doc """
Use this method to set the result of an interaction with a Web App and send a corresponding message on behalf of the user to the chat from which the query originated.  
On success, a SentWebAppMessage object is returned.
## Parameters
  * `web_app_query_id` - Unique identifier for the query to be answered
  * `result` - A JSON-serialized object describing the message to be sent
""".
-doc (#{group=><<"Inline Mode">>,since=><<"6.0">>}).
-spec answerWebAppQuery(Pool :: pool_name(), Req :: #{web_app_query_id := binary(), result := 'InlineQueryResult'()}, Async :: boolean()) -> Result :: result('SentWebAppMessage'()).
answerWebAppQuery(Pool, #{web_app_query_id:=_,result:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"answerWebAppQuery">>, Req, Async}).
-doc (#{equiv=>answerWebAppQuery(Pool, Req, false),since=><<"6.0">>,group=><<"Sync Request">>}).
answerWebAppQuery(Pool, Req) -> answerWebAppQuery(Pool, Req, false).


-doc """
Stores a message that can be sent by a user of a Mini App.  
Returns a PreparedInlineMessage object.
## Parameters
  * `user_id` - Unique identifier of the target user that can use the prepared message
  * `result` - A JSON-serialized object describing the message to be sent
  * `allow_user_chats` - Pass True if the message can be sent to private chats with users
  * `allow_bot_chats` - Pass True if the message can be sent to private chats with bots
  * `allow_group_chats` - Pass True if the message can be sent to group and supergroup chats
  * `allow_channel_chats` - Pass True if the message can be sent to channel chats
""".
-doc (#{group=><<"Message">>,since=><<"8.0">>}).
-spec savePreparedInlineMessage(Pool :: pool_name(), Req :: #{user_id := integer(), result := 'InlineQueryResult'(), allow_user_chats => boolean(), allow_bot_chats => boolean(), allow_group_chats => boolean(), allow_channel_chats => boolean()}, Async :: boolean()) -> Result :: result('PreparedInlineMessage'()).
savePreparedInlineMessage(Pool, #{user_id:=_,result:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"savePreparedInlineMessage">>, Req, Async}).
-doc (#{equiv=>savePreparedInlineMessage(Pool, Req, false),since=><<"8.0">>,group=><<"Sync Request">>}).
savePreparedInlineMessage(Pool, Req) -> savePreparedInlineMessage(Pool, Req, false).


-doc """
Use this method to send invoices.  
On success, the sent Message is returned.
## Parameters
  * `chat_id` - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  * `message_thread_id` - Unique identifier for the target message thread (topic) of a forum; for forum supergroups and private chats of bots with forum topic mode enabled only
  * `direct_messages_topic_id` - Identifier of the direct messages topic to which the message will be sent; required if the message is sent to a direct messages chat
  * `title` - Product name, 1-32 characters
  * `description` - Product description, 1-255 characters
  * `payload` - Bot-defined invoice payload, 1-128 bytes. This will not be displayed to the user, use it for your internal processes.
  * `provider_token` - Payment provider token, obtained via @BotFather. Pass an empty string for payments in Telegram Stars.
  * `currency` - Three-letter ISO 4217 currency code, see more on currencies. Pass “XTR” for payments in Telegram Stars.
  * `prices` - Price breakdown, a JSON-serialized list of components (e.g. product price, tax, discount, delivery cost, delivery tax, bonus, etc.). Must contain exactly one item for payments in Telegram Stars.
  * `max_tip_amount` - The maximum accepted amount for tips in the smallest units of the currency (integer, not float/double). For example, for a maximum tip of US$ 1.45 pass max_tip_amount = 145. See the exp parameter in currencies.json, it shows the number of digits past the decimal point for each currency (2 for the majority of currencies). Defaults to 0. Not supported for payments in Telegram Stars.
  * `suggested_tip_amounts` - A JSON-serialized array of suggested amounts of tips in the smallest units of the currency (integer, not float/double). At most 4 suggested tip amounts can be specified. The suggested tip amounts must be positive, passed in a strictly increased order and must not exceed max_tip_amount.
  * `start_parameter` - Unique deep-linking parameter. If left empty, forwarded copies of the sent message will have a Pay button, allowing multiple users to pay directly from the forwarded message, using the same invoice. If non-empty, forwarded copies of the sent message will have a URL button with a deep link to the bot (instead of a Pay button), with the value used as the start parameter
  * `provider_data` - JSON-serialized data about the invoice, which will be shared with the payment provider. A detailed description of required fields should be provided by the payment provider.
  * `photo_url` - URL of the product photo for the invoice. Can be a photo of the goods or a marketing image for a service. People like it better when they see what they are paying for.
  * `photo_size` - Photo size in bytes
  * `photo_width` - Photo width
  * `photo_height` - Photo height
  * `need_name` - Pass True if you require the user's full name to complete the order. Ignored for payments in Telegram Stars.
  * `need_phone_number` - Pass True if you require the user's phone number to complete the order. Ignored for payments in Telegram Stars.
  * `need_email` - Pass True if you require the user's email address to complete the order. Ignored for payments in Telegram Stars.
  * `need_shipping_address` - Pass True if you require the user's shipping address to complete the order. Ignored for payments in Telegram Stars.
  * `send_phone_number_to_provider` - Pass True if the user's phone number should be sent to the provider. Ignored for payments in Telegram Stars.
  * `send_email_to_provider` - Pass True if the user's email address should be sent to the provider. Ignored for payments in Telegram Stars.
  * `is_flexible` - Pass True if the final price depends on the shipping method. Ignored for payments in Telegram Stars.
  * `disable_notification` - Sends the message silently. Users will receive a notification with no sound.
  * `protect_content` - Protects the contents of the sent message from forwarding and saving
  * `allow_paid_broadcast` - Pass True to allow up to 1000 messages per second, ignoring broadcasting limits for a fee of 0.1 Telegram Stars per message. The relevant Stars will be withdrawn from the bot's balance
  * `message_effect_id` - Unique identifier of the message effect to be added to the message; for private chats only
  * `suggested_post_parameters` - A JSON-serialized object containing the parameters of the suggested post to send; for direct messages chats only. If the message is sent as a reply to another suggested post, then that suggested post is automatically declined.
  * `reply_parameters` - Description of the message to reply to
  * `reply_markup` - A JSON-serialized object for an inline keyboard. If empty, one 'Pay total price' button will be shown. If not empty, the first button must be a Pay button.
""".
-doc (#{group=><<"Payments">>,since=><<"3.0">>}).
-spec sendInvoice(Pool :: pool_name(), Req :: #{chat_id := integer() | binary(), message_thread_id => integer(), direct_messages_topic_id => integer(), title := binary(), description := binary(), payload := binary(), provider_token => binary(), currency := binary(), prices := nonempty_list('LabeledPrice'()), max_tip_amount => integer(), suggested_tip_amounts => nonempty_list(integer()), start_parameter => binary(), provider_data => binary(), photo_url => binary(), photo_size => integer(), photo_width => integer(), photo_height => integer(), need_name => boolean(), need_phone_number => boolean(), need_email => boolean(), need_shipping_address => boolean(), send_phone_number_to_provider => boolean(), send_email_to_provider => boolean(), is_flexible => boolean(), disable_notification => boolean(), protect_content => boolean(), allow_paid_broadcast => boolean(), message_effect_id => binary(), suggested_post_parameters => 'SuggestedPostParameters'(), reply_parameters => 'ReplyParameters'(), reply_markup => 'InlineKeyboardMarkup'()}, Async :: boolean()) -> Result :: result('Message'()).
sendInvoice(Pool, #{chat_id:=_,title:=_,description:=_,payload:=_,currency:=_,prices:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"sendInvoice">>, Req, Async}).
-doc (#{equiv=>sendInvoice(Pool, Req, false),since=><<"3.0">>,group=><<"Sync Request">>}).
sendInvoice(Pool, Req) -> sendInvoice(Pool, Req, false).


-doc """
Use this method to create a link for an invoice.  
Returns the created invoice link as String on success.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the link will be created. For payments in Telegram Stars only.
  * `title` - Product name, 1-32 characters
  * `description` - Product description, 1-255 characters
  * `payload` - Bot-defined invoice payload, 1-128 bytes. This will not be displayed to the user, use it for your internal processes.
  * `provider_token` - Payment provider token, obtained via @BotFather. Pass an empty string for payments in Telegram Stars.
  * `currency` - Three-letter ISO 4217 currency code, see more on currencies. Pass “XTR” for payments in Telegram Stars.
  * `prices` - Price breakdown, a JSON-serialized list of components (e.g. product price, tax, discount, delivery cost, delivery tax, bonus, etc.). Must contain exactly one item for payments in Telegram Stars.
  * `subscription_period` - The number of seconds the subscription will be active for before the next payment. The currency must be set to “XTR” (Telegram Stars) if the parameter is used. Currently, it must always be 2592000 (30 days) if specified. Any number of subscriptions can be active for a given bot at the same time, including multiple concurrent subscriptions from the same user. Subscription price must no exceed 10000 Telegram Stars.
  * `max_tip_amount` - The maximum accepted amount for tips in the smallest units of the currency (integer, not float/double). For example, for a maximum tip of US$ 1.45 pass max_tip_amount = 145. See the exp parameter in currencies.json, it shows the number of digits past the decimal point for each currency (2 for the majority of currencies). Defaults to 0. Not supported for payments in Telegram Stars.
  * `suggested_tip_amounts` - A JSON-serialized array of suggested amounts of tips in the smallest units of the currency (integer, not float/double). At most 4 suggested tip amounts can be specified. The suggested tip amounts must be positive, passed in a strictly increased order and must not exceed max_tip_amount.
  * `provider_data` - JSON-serialized data about the invoice, which will be shared with the payment provider. A detailed description of required fields should be provided by the payment provider.
  * `photo_url` - URL of the product photo for the invoice. Can be a photo of the goods or a marketing image for a service.
  * `photo_size` - Photo size in bytes
  * `photo_width` - Photo width
  * `photo_height` - Photo height
  * `need_name` - Pass True if you require the user's full name to complete the order. Ignored for payments in Telegram Stars.
  * `need_phone_number` - Pass True if you require the user's phone number to complete the order. Ignored for payments in Telegram Stars.
  * `need_email` - Pass True if you require the user's email address to complete the order. Ignored for payments in Telegram Stars.
  * `need_shipping_address` - Pass True if you require the user's shipping address to complete the order. Ignored for payments in Telegram Stars.
  * `send_phone_number_to_provider` - Pass True if the user's phone number should be sent to the provider. Ignored for payments in Telegram Stars.
  * `send_email_to_provider` - Pass True if the user's email address should be sent to the provider. Ignored for payments in Telegram Stars.
  * `is_flexible` - Pass True if the final price depends on the shipping method. Ignored for payments in Telegram Stars.
""".
-doc (#{group=><<"Payments">>,since=><<"6.1">>}).
-spec createInvoiceLink(Pool :: pool_name(), Req :: #{business_connection_id => binary(), title := binary(), description := binary(), payload := binary(), provider_token => binary(), currency := binary(), prices := nonempty_list('LabeledPrice'()), subscription_period => integer(), max_tip_amount => integer(), suggested_tip_amounts => nonempty_list(integer()), provider_data => binary(), photo_url => binary(), photo_size => integer(), photo_width => integer(), photo_height => integer(), need_name => boolean(), need_phone_number => boolean(), need_email => boolean(), need_shipping_address => boolean(), send_phone_number_to_provider => boolean(), send_email_to_provider => boolean(), is_flexible => boolean()}, Async :: boolean()) -> Result :: result(binary()).
createInvoiceLink(Pool, #{title:=_,description:=_,payload:=_,currency:=_,prices:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"createInvoiceLink">>, Req, Async}).
-doc (#{equiv=>createInvoiceLink(Pool, Req, false),since=><<"6.1">>,group=><<"Sync Request">>}).
createInvoiceLink(Pool, Req) -> createInvoiceLink(Pool, Req, false).


-doc """
If you sent an invoice requesting a shipping address and the parameter is_flexible was specified, the Bot API will send an Update with a shipping_query field to the bot.  
Use this method to reply to shipping queries.  
On success, True is returned.
## Parameters
  * `shipping_query_id` - Unique identifier for the query to be answered
  * `ok` - Pass True if delivery to the specified address is possible and False if there are any problems (for example, if delivery to the specified address is not possible)
  * `shipping_options` - Required if ok is True. A JSON-serialized array of available shipping options.
  * `error_message` - Required if ok is False. Error message in human readable form that explains why it is impossible to complete the order (e.g. “Sorry, delivery to your desired address is unavailable”). Telegram will display this message to the user.
""".
-doc (#{group=><<"Payments">>,since=><<"3.0">>}).
-spec answerShippingQuery(Pool :: pool_name(), Req :: #{shipping_query_id := binary(), ok := boolean(), shipping_options => nonempty_list('ShippingOption'()), error_message => binary()}, Async :: boolean()) -> Result :: result(true).
answerShippingQuery(Pool, #{shipping_query_id:=_,ok:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"answerShippingQuery">>, Req, Async}).
-doc (#{equiv=>answerShippingQuery(Pool, Req, false),since=><<"3.0">>,group=><<"Sync Request">>}).
answerShippingQuery(Pool, Req) -> answerShippingQuery(Pool, Req, false).


-doc """
Once the user has confirmed their payment and shipping details, the Bot API sends the final confirmation in the form of an Update with the field pre_checkout_query.  
Use this method to respond to such pre-checkout queries.  
On success, True is returned.  
Note: The Bot API must receive an answer within 10 seconds after the pre-checkout query was sent.
## Parameters
  * `pre_checkout_query_id` - Unique identifier for the query to be answered
  * `ok` - Specify True if everything is alright (goods are available, etc.) and the bot is ready to proceed with the order. Use False if there are any problems.
  * `error_message` - Required if ok is False. Error message in human readable form that explains the reason for failure to proceed with the checkout (e.g. Sorry, somebody just bought the last of our amazing black T-shirts while you were busy filling out your payment details. Please choose a different color or garment!). Telegram will display this message to the user.
""".
-doc (#{group=><<"Payments">>,since=><<"3.0">>}).
-spec answerPreCheckoutQuery(Pool :: pool_name(), Req :: #{pre_checkout_query_id := binary(), ok := boolean(), error_message => binary()}, Async :: boolean()) -> Result :: result(true).
answerPreCheckoutQuery(Pool, #{pre_checkout_query_id:=_,ok:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"answerPreCheckoutQuery">>, Req, Async}).
-doc (#{equiv=>answerPreCheckoutQuery(Pool, Req, false),since=><<"3.0">>,group=><<"Sync Request">>}).
answerPreCheckoutQuery(Pool, Req) -> answerPreCheckoutQuery(Pool, Req, false).


-doc """
A method to get the current Telegram Stars balance of the bot.  
Requires no parameters.  
On success, returns a StarAmount object.
""".
-doc (#{group=><<"Payments">>,since=><<"9.1">>}).
-spec getMyStarBalance(Pool :: pool_name(), Req :: empty_map(), Async :: boolean()) -> Result :: result('StarAmount'()).
getMyStarBalance(Pool, #{} = Req, Async) ->wpool:call(Pool, {raw, <<"getMyStarBalance">>, Req, Async}).
-doc (#{equiv=>getMyStarBalance(Pool, Req, false),since=><<"9.1">>,group=><<"Sync Request">>}).
getMyStarBalance(Pool, Req) -> getMyStarBalance(Pool, Req, false).


-doc """
Returns the bot's Telegram Star transactions in chronological order.  
On success, returns a StarTransactions object.
## Parameters
  * `offset` - Number of transactions to skip in the response
  * `limit` - The maximum number of transactions to be retrieved. Values between 1-100 are accepted. Defaults to 100.
""".
-doc (#{group=><<"Payments">>,since=><<"7.5">>}).
-spec getStarTransactions(Pool :: pool_name(), Req :: #{offset => integer(), limit => integer()}, Async :: boolean()) -> Result :: result('StarTransactions'()).
getStarTransactions(Pool, #{} = Req, Async) ->wpool:call(Pool, {raw, <<"getStarTransactions">>, Req, Async}).
-doc (#{equiv=>getStarTransactions(Pool, Req, false),since=><<"7.5">>,group=><<"Sync Request">>}).
getStarTransactions(Pool, Req) -> getStarTransactions(Pool, Req, false).


-doc """
Refunds a successful payment in Telegram Stars.  
Returns True on success.
## Parameters
  * `user_id` - Identifier of the user whose payment will be refunded
  * `telegram_payment_charge_id` - Telegram payment identifier
""".
-doc (#{group=><<"Payments">>,since=><<"7.4">>}).
-spec refundStarPayment(Pool :: pool_name(), Req :: #{user_id := integer(), telegram_payment_charge_id := binary()}, Async :: boolean()) -> Result :: result(true).
refundStarPayment(Pool, #{user_id:=_,telegram_payment_charge_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"refundStarPayment">>, Req, Async}).
-doc (#{equiv=>refundStarPayment(Pool, Req, false),since=><<"7.4">>,group=><<"Sync Request">>}).
refundStarPayment(Pool, Req) -> refundStarPayment(Pool, Req, false).


-doc """
Allows the bot to cancel or re-enable extension of a subscription paid in Telegram Stars.  
Returns True on success.
## Parameters
  * `user_id` - Identifier of the user whose subscription will be edited
  * `telegram_payment_charge_id` - Telegram payment identifier for the subscription
  * `is_canceled` - Pass True to cancel extension of the user subscription; the subscription must be active up to the end of the current subscription period. Pass False to allow the user to re-enable a subscription that was previously canceled by the bot.
""".
-doc (#{group=><<"Payments">>,since=><<"8.0">>}).
-spec editUserStarSubscription(Pool :: pool_name(), Req :: #{user_id := integer(), telegram_payment_charge_id := binary(), is_canceled := boolean()}, Async :: boolean()) -> Result :: result(true).
editUserStarSubscription(Pool, #{user_id:=_,telegram_payment_charge_id:=_,is_canceled:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"editUserStarSubscription">>, Req, Async}).
-doc (#{equiv=>editUserStarSubscription(Pool, Req, false),since=><<"8.0">>,group=><<"Sync Request">>}).
editUserStarSubscription(Pool, Req) -> editUserStarSubscription(Pool, Req, false).


-doc """
Informs a user that some of the Telegram Passport elements they provided contains errors.  
The user will not be able to re-submit their Passport to you until the errors are fixed (the contents of the field for which you returned the error must change).  
Returns True on success.  
Use this if the data submitted by the user doesn't satisfy the standards your service requires for any reason.  
For example, if a birthday date seems invalid, a submitted document is blurry, a scan shows evidence of tampering, etc.  
Supply some details in the error message to make sure the user knows how to correct the issues.
## Parameters
  * `user_id` - User identifier
  * `errors` - A JSON-serialized array describing the errors
""".
-doc (#{group=><<"Passport">>,since=><<"">>}).
-spec setPassportDataErrors(Pool :: pool_name(), Req :: #{user_id := integer(), errors := nonempty_list('PassportElementError'())}, Async :: boolean()) -> Result :: result(true).
setPassportDataErrors(Pool, #{user_id:=_,errors:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"setPassportDataErrors">>, Req, Async}).
-doc (#{equiv=>setPassportDataErrors(Pool, Req, false),since=><<"">>,group=><<"Sync Request">>}).
setPassportDataErrors(Pool, Req) -> setPassportDataErrors(Pool, Req, false).


-doc """
Use this method to send a game.  
On success, the sent Message is returned.
## Parameters
  * `business_connection_id` - Unique identifier of the business connection on behalf of which the message will be sent
  * `chat_id` - Unique identifier for the target chat. Games can't be sent to channel direct messages chats and channel chats.
  * `message_thread_id` - Unique identifier for the target message thread (topic) of a forum; for forum supergroups and private chats of bots with forum topic mode enabled only
  * `game_short_name` - Short name of the game, serves as the unique identifier for the game. Set up your games via @BotFather.
  * `disable_notification` - Sends the message silently. Users will receive a notification with no sound.
  * `protect_content` - Protects the contents of the sent message from forwarding and saving
  * `allow_paid_broadcast` - Pass True to allow up to 1000 messages per second, ignoring broadcasting limits for a fee of 0.1 Telegram Stars per message. The relevant Stars will be withdrawn from the bot's balance
  * `message_effect_id` - Unique identifier of the message effect to be added to the message; for private chats only
  * `reply_parameters` - Description of the message to reply to
  * `reply_markup` - A JSON-serialized object for an inline keyboard. If empty, one 'Play game_title' button will be shown. If not empty, the first button must launch the game.
""".
-doc (#{group=><<"Games">>,since=><<"2.2">>}).
-spec sendGame(Pool :: pool_name(), Req :: #{business_connection_id => binary(), chat_id := integer(), message_thread_id => integer(), game_short_name := binary(), disable_notification => boolean(), protect_content => boolean(), allow_paid_broadcast => boolean(), message_effect_id => binary(), reply_parameters => 'ReplyParameters'(), reply_markup => 'InlineKeyboardMarkup'()}, Async :: boolean()) -> Result :: result('Message'()).
sendGame(Pool, #{chat_id:=_,game_short_name:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"sendGame">>, Req, Async}).
-doc (#{equiv=>sendGame(Pool, Req, false),since=><<"2.2">>,group=><<"Sync Request">>}).
sendGame(Pool, Req) -> sendGame(Pool, Req, false).


-doc """
Use this method to set the score of the specified user in a game message.  
On success, if the message is not an inline message, the Message is returned, otherwise True is returned.  
Returns an error, if the new score is not greater than the user's current score in the chat and force is False.
## Parameters
  * `user_id` - User identifier
  * `score` - New score, must be non-negative
  * `force` - Pass True if the high score is allowed to decrease. This can be useful when fixing mistakes or banning cheaters
  * `disable_edit_message` - Pass True if the game message should not be automatically edited to include the current scoreboard
  * `chat_id` - Required if inline_message_id is not specified. Unique identifier for the target chat
  * `message_id` - Required if inline_message_id is not specified. Identifier of the sent message
  * `inline_message_id` - Required if chat_id and message_id are not specified. Identifier of the inline message
""".
-doc (#{group=><<"Games">>,since=><<"2.2">>}).
-spec setGameScore(Pool :: pool_name(), Req :: #{user_id := integer(), score := integer(), force => boolean(), disable_edit_message => boolean(), chat_id => integer(), message_id => integer(), inline_message_id => binary()}, Async :: boolean()) -> Result :: result(true | 'Message'()).
setGameScore(Pool, #{user_id:=_,score:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"setGameScore">>, Req, Async}).
-doc (#{equiv=>setGameScore(Pool, Req, false),since=><<"2.2">>,group=><<"Sync Request">>}).
setGameScore(Pool, Req) -> setGameScore(Pool, Req, false).


-doc """
Use this method to get data for high score tables.  
Will return the score of the specified user and several of their neighbors in a game.  
Returns an Array of GameHighScore objects.  
This method will currently return scores for the target user, plus two of their closest neighbors on each side.  
Will also return the top three users if the user and their neighbors are not among them.  
Please note that this behavior is subject to change.
## Parameters
  * `user_id` - Target user id
  * `chat_id` - Required if inline_message_id is not specified. Unique identifier for the target chat
  * `message_id` - Required if inline_message_id is not specified. Identifier of the sent message
  * `inline_message_id` - Required if chat_id and message_id are not specified. Identifier of the inline message
""".
-doc (#{group=><<"Games">>,since=><<"2.2">>}).
-spec getGameHighScores(Pool :: pool_name(), Req :: #{user_id := integer(), chat_id => integer(), message_id => integer(), inline_message_id => binary()}, Async :: boolean()) -> Result :: result(nonempty_list('GameHighScore'())).
getGameHighScores(Pool, #{user_id:=_} = Req, Async) ->wpool:call(Pool, {raw, <<"getGameHighScores">>, Req, Async}).
-doc (#{equiv=>getGameHighScores(Pool, Req, false),since=><<"2.2">>,group=><<"Sync Request">>}).
getGameHighScores(Pool, Req) -> getGameHighScores(Pool, Req, false).

