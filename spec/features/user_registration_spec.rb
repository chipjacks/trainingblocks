require 'rails_helper'

describe 'User registration', type: :feature do
  scenario 'Is on homepage and wants to sign up' do
    visit root_path
    click_link 'Sign up'
    expect(find('h2')).to have_content('Sign up')
  end

  scenario 'User creates a new account' do
    visit new_user_registration_path

    fill_in 'user[email]', with: 'jimbo@example.com'
    fill_in 'user[password]', with: 'password'
    fill_in 'user[password_confirmation]', with: 'password'
    click_button 'Sign up'

    expect(page).to have_content('A message with a confirmation link')
  end

  scenario "User doesn't enter an email" do
    visit new_user_registration_path

    fill_in 'user[email]', with: ''
    fill_in 'user[password]', with: 'password'
    fill_in 'user[password_confirmation]', with: 'password'
    click_button 'Sign up'

    expect(page).to have_content("Email can't be blank")
  end

  scenario 'New user signs in from a Strava account' do
    visit new_user_registration_path
    click_link 'Sign in with Strava'

    expect(page).to have_content('Please enter an email')

    fill_in 'user[email]', with: 'test@example.com'
    fill_in 'user[password]', with: 'password'
    fill_in 'user[password_confirmation]', with: 'password'

    click_button 'Sign up'
    expect(page).to have_content('A message with a confirmation link')
    expect(User.first.auth_token).to be_truthy
  end

  scenario 'Existing user without email signs in from a Strava account' do
    user = FactoryBot.build(:user, :strava)
    user.email = nil
    user.password = nil
    user.confirmed_at = nil
    user.save(validate: false)

    visit new_user_registration_path
    click_link 'Sign in with Strava'

    expect(page).to have_content('Please enter an email')

    fill_in 'user[email]', with: 'test@example.com'
    fill_in 'user[password]', with: 'password'
    fill_in 'user[password_confirmation]', with: 'password'

    click_button 'Sign up'
    expect(page).to have_content('A message with a confirmation link')
    expect(user.reload.unconfirmed_email).to eq('test@example.com')
  end

  context 'Is already signed in' do
    let(:user) { FactoryBot.create(:user) }

    before :each do
      sign_in user
    end

    scenario 'User updates email address' do
      visit edit_user_registration_path

      fill_in 'user[email]', with: 'example2@example.com'
      fill_in 'user[current_password]', with: user.password
      click_button 'Update'

      expect(page).to have_content('You updated your account successfully')
    end
  end
end
