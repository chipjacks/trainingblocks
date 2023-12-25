require 'rails_helper'

RSpec.feature 'User authentication', type: :feature do
  context 'Existing user with email only' do
    before :each do
      @user = FactoryBot.create(:user)
    end

    scenario 'signs in to a Strava account for first time' do
      visit new_user_session_path
      click_link 'Connect with Strava'

      click_link 'Log in'
      fill_in 'user[email]', with: @user.email
      fill_in 'user[password]', with: @user.password
      expect(InitialStravaImportJob).to receive(:perform_now)
      click_button 'Log in'

      expect(page).to have_selector('#elm-main')
      @user.reload
      expect(@user.auth_token).to be_truthy
    end
  end

  context 'Existing user with email and strava' do
    before :each do
      @user = FactoryBot.create(:user, :strava)
    end

    scenario 'Signs in with email' do
      visit new_user_session_path
      fill_in 'user[email]', with: @user.email
      fill_in 'user[password]', with: @user.password
      click_button 'Log in'
      expect(page).to have_content('Signed in successfully.')
    end

    scenario 'Signs in with Strava' do
      visit new_user_session_path
      click_link 'Connect with Strava'
      expect(page).to have_selector('#elm-main')
    end
  end
end
